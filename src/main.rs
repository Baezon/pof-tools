//     This disables the console that pops up if you run the program on windows but also supresses output on stdout
// VVV wasn't sure how to handle it so i leave it on for debug but turn it off for release
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]
#![allow(clippy::useless_format)]
#[macro_use]
extern crate log;
extern crate nalgebra_glm as glm;
extern crate simplelog;

use crate::ui::{
    DisplayMode, DockingSelection, EyeSelection, GlowSelection, InsigniaSelection, PathSelection, SpecialPointSelection, SubObjectSelection,
    TextureSelection, ThrusterSelection, TurretSelection, WeaponSelection,
};
use egui::{Color32, RichText, TextEdit};
use glium::{
    glutin::{self, event::WindowEvent, window::Icon},
    texture::SrgbTexture2d,
    BlendingFunction, Display, IndexBuffer, LinearBlendingFactor, VertexBuffer,
};
use glm::Mat4x4;
use native_dialog::FileDialog;
use pof::{
    BspData, BspNode, Insignia, Model, NormalId, ObjVec, ObjectId, Parser, PolyVertex, Polygon, ShieldData, SubObject, TextureId, Texturing, Vec3d,
    VertexId,
};
use simplelog::*;
use std::{
    collections::HashMap,
    fs::File,
    io::{Cursor, Read},
    path::PathBuf,
    sync::mpsc::TryRecvError,
};
use ui::{PofToolsGui, Set::*, TreeSelection};

mod primitives;
mod ui;
mod ui_properties_panel;

fn create_display(event_loop: &glutin::event_loop::EventLoop<()>) -> glium::Display {
    let window_builder = glutin::window::WindowBuilder::new()
        .with_resizable(true)
        .with_inner_size(glutin::dpi::LogicalSize { width: 800.0f32, height: 600.0f32 })
        .with_title(format!("Pof Tools v{}", POF_TOOLS_VERSION))
        .with_window_icon(Some(Icon::from_rgba(include_bytes!("icon.raw").to_vec(), 32, 32).unwrap()));

    let context_builder = glutin::ContextBuilder::new()
        .with_depth_buffer(0)
        .with_srgb(true)
        .with_stencil_buffer(0)
        .with_vsync(true)
        .with_depth_buffer(24);

    glium::Display::new(window_builder, context_builder, event_loop).unwrap()
}

const ADDITIVE_BLEND: glium::draw_parameters::Blend = glium::draw_parameters::Blend {
    color: BlendingFunction::Addition {
        source: LinearBlendingFactor::One,
        destination: LinearBlendingFactor::One,
    },
    alpha: BlendingFunction::Addition {
        source: LinearBlendingFactor::One,
        destination: LinearBlendingFactor::One,
    },
    constant_value: (0.0, 0.0, 1.0, 0.0),
};

struct GlLollipops {
    color: [f32; 4], // RGBA
    lolly_vertices: VertexBuffer<InstanceMatrix>,
    stick_vertices: VertexBuffer<Vertex>,
    stick_indices: glium::index::NoIndices,
}

struct GlLollipopsBuilder {
    color: [f32; 4], // RGBA
    lolly_vertices: Vec<InstanceMatrix>,
    stick_vertices: Vec<Vertex>,
}
impl GlLollipopsBuilder {
    fn new(color: [f32; 4]) -> Self {
        Self { color, lolly_vertices: vec![], stick_vertices: vec![] }
    }

    fn push(&mut self, vertex: Vec3d, normal: Vec3d, radius: f32) {
        let mut matrix = glm::translation(&glm::vec3(vertex.x, vertex.y, vertex.z));
        matrix = glm::scale(&matrix, &glm::vec3(radius, radius, radius));
        self.lolly_vertices.push(InstanceMatrix { world_matrix: matrix.into() });

        self.stick_vertices.push(Vertex { position: vertex.to_tuple(), uv: (0.0, 0.0) });
        self.stick_vertices
            .push(Vertex { position: (vertex + normal).to_tuple(), uv: (0.0, 0.0) });
    }

    fn finish(&self, display: &Display) -> GlLollipops {
        GlLollipops {
            color: self.color,
            lolly_vertices: glium::VertexBuffer::new(display, &self.lolly_vertices).unwrap(),
            stick_vertices: glium::VertexBuffer::new(display, &self.stick_vertices).unwrap(),
            stick_indices: glium::index::NoIndices(glium::index::PrimitiveType::LinesList),
        }
    }
}

fn build_lollipops(colors: &[[f32; 4]], display: &Display, iter: impl Iterator<Item = (Vec3d, Vec3d, f32, usize)>) -> Vec<GlLollipops> {
    let mut builders: Vec<_> = colors.iter().map(|c| GlLollipopsBuilder::new(*c)).collect();
    for (vertex, normal, radius, selection) in iter {
        builders[selection].push(vertex, normal, radius)
    }
    builders.into_iter().map(|builder| builder.finish(display)).collect()
}
struct GlBufferedShield {
    vertices: VertexBuffer<Vertex>,
    normals: VertexBuffer<Normal>,
    indices: IndexBuffer<u32>,
}
impl GlBufferedShield {
    fn new(display: &Display, shield_data: &ShieldData) -> GlBufferedShield {
        info!("Building buffer for the shield");
        let mut vertices = vec![];
        let mut normals = vec![];
        let mut indices = vec![];

        for poly in &shield_data.polygons {
            vertices.push(Vertex {
                position: shield_data.verts[poly.verts.0 .0 as usize].to_tuple(),
                uv: (0.0, 0.0),
            });
            normals.push(Normal { normal: poly.normal.to_tuple() });
            indices.push(indices.len() as u32);

            vertices.push(Vertex {
                position: shield_data.verts[poly.verts.1 .0 as usize].to_tuple(),
                uv: (0.0, 0.0),
            });
            normals.push(Normal { normal: poly.normal.to_tuple() });
            indices.push(indices.len() as u32);

            vertices.push(Vertex {
                position: shield_data.verts[poly.verts.2 .0 as usize].to_tuple(),
                uv: (0.0, 0.0),
            });
            normals.push(Normal { normal: poly.normal.to_tuple() });
            indices.push(indices.len() as u32);
        }

        GlBufferedShield {
            vertices: glium::VertexBuffer::new(display, &vertices).unwrap(),
            normals: glium::VertexBuffer::new(display, &normals).unwrap(),
            indices: glium::IndexBuffer::new(display, glium::index::PrimitiveType::TrianglesList, &indices).unwrap(),
        }
    }
}

struct GlBufferedInsignia {
    vertices: VertexBuffer<Vertex>,
    normals: VertexBuffer<Normal>,
    indices: IndexBuffer<u32>,
}
impl GlBufferedInsignia {
    fn new(display: &Display, insignia: &Insignia) -> GlBufferedInsignia {
        info!("Building buffer for a LOD {} insignia ", insignia.detail_level);
        let mut vertices = vec![];
        let mut normals = vec![];
        let mut indices = vec![];

        for (vert1, vert2, vert3) in &insignia.faces {
            let [v1, v2, v3] = [vert1, vert2, vert3].map(|i| nalgebra_glm::Vec3::from(insignia.vertices[i.vertex_id.0 as usize]));
            let normal: Vec3d = (v2 - v1).cross(&(v3 - v1)).normalize().into();

            vertices.push(Vertex {
                position: insignia.vertices[vert1.vertex_id.0 as usize].to_tuple(),
                uv: vert1.uv,
            });
            normals.push(Normal { normal: normal.to_tuple() });
            indices.push(indices.len() as u32);

            vertices.push(Vertex {
                position: insignia.vertices[vert2.vertex_id.0 as usize].to_tuple(),
                uv: vert2.uv,
            });
            normals.push(Normal { normal: normal.to_tuple() });
            indices.push(indices.len() as u32);

            vertices.push(Vertex {
                position: insignia.vertices[vert3.vertex_id.0 as usize].to_tuple(),
                uv: vert3.uv,
            });
            normals.push(Normal { normal: normal.to_tuple() });
            indices.push(indices.len() as u32);
        }

        GlBufferedInsignia {
            vertices: glium::VertexBuffer::new(display, &vertices).unwrap(),
            normals: glium::VertexBuffer::new(display, &normals).unwrap(),
            indices: glium::IndexBuffer::new(display, glium::index::PrimitiveType::TrianglesList, &indices).unwrap(),
        }
    }
}

#[derive(Default)]
struct GlObjectBuilder {
    vertices: Vec<Vertex>,
    normals: Vec<Normal>,
    map: HashMap<(VertexId, NormalId, u32, u32), u32>,
}

impl GlObjectBuilder {
    fn get_index(&mut self, bsp_data: &BspData, vert: &PolyVertex) -> u32 {
        *self
            .map
            .entry((vert.vertex_id, vert.normal_id, vert.uv.0.to_bits(), vert.uv.1.to_bits()))
            .or_insert_with(|| {
                let n = self.vertices.len();
                self.vertices.push(Vertex {
                    position: bsp_data.verts[vert.vertex_id.0 as usize].to_tuple(),
                    uv: vert.uv,
                });
                self.normals.push(Normal { normal: bsp_data.norms[vert.normal_id.0 as usize].to_tuple() });
                n.try_into().unwrap()
            })
    }
}

#[derive(Default)]
struct GlObjectsBuilder {
    inner: GlObjectBuilder,
    indices: Vec<u32>,
    wireframe_indices: Vec<u32>,
}

impl GlObjectsBuilder {
    fn push(&mut self, bsp_data: &BspData, poly: &Polygon) {
        // need to triangulate possibly
        // we'll make tris like this 0,1,2 .. 0,2,3 .. 0,3,4... etc
        if let [v0, v1, v2, remainder @ ..] = &*poly.verts {
            // get indices for v0, v1, v2
            let index0 = self.inner.get_index(bsp_data, v0);
            let index1 = self.inner.get_index(bsp_data, v1);
            let mut index = self.inner.get_index(bsp_data, v2);

            if remainder.is_empty() {
                // special case for tris which are common
                // triangle is v0-v1-v2
                self.indices.extend_from_slice(&[index0, index1, index]);
                // don't create wireframe unless we have previously encountered a quad
                if !self.wireframe_indices.is_empty() {
                    // wireframe is [v0-v1, v1-v2, v2-v0]
                    self.wireframe_indices.extend_from_slice(&[index0, index1, index1, index, index, index0]);
                }
            } else {
                // if this is not a tri
                if self.wireframe_indices.is_empty() {
                    // lazy initialization of wireframe object
                    for vs in self.indices.chunks_exact(3) {
                        // everything prior is a tri, so wireframe is [v0-v1, v1-v2, v2-v0]
                        self.wireframe_indices.extend_from_slice(&[vs[0], vs[1], vs[1], vs[2], vs[2], vs[0]]);
                    }
                }
                // first tri is v0-v1-v2
                self.indices.extend_from_slice(&[index0, index1, index]);
                // start drawing the outline with [v0-v1, v1-v2]
                self.wireframe_indices.extend_from_slice(&[index0, index1, index1, index]);
                // for each pair of verts in the remainder make a tri of [v0, v(i-1), vi]
                for vi in remainder {
                    let index2 = self.inner.get_index(bsp_data, vi);
                    // tri is v0-v(i-1)-vi
                    self.indices.extend_from_slice(&[index0, index, index2]);
                    // wireframe gets an extra line v(i-1)-vi
                    self.wireframe_indices.extend_from_slice(&[index, index2]);
                    index = index2;
                }
                // finish the wireframe polygon with vn-v0
                self.wireframe_indices.extend_from_slice(&[index, index0]);
            }
        }
    }

    fn finish(self, display: &Display, object: &SubObject, texture_id: Option<TextureId>, out: &mut Vec<GlObjectBuffer>) {
        if !self.indices.is_empty() {
            info!("Built buffer for subobj {}", object.name);
            out.push(GlObjectBuffer {
                texture_id,
                vertices: glium::VertexBuffer::new(display, &self.inner.vertices).unwrap(),
                normals: glium::VertexBuffer::new(display, &self.inner.normals).unwrap(),
                indices: glium::IndexBuffer::new(display, glium::index::PrimitiveType::TrianglesList, &self.indices).unwrap(),
                wireframe_indices: if self.wireframe_indices.is_empty() {
                    None
                } else {
                    Some(glium::IndexBuffer::new(display, glium::index::PrimitiveType::LinesList, &self.wireframe_indices).unwrap())
                },
                tint_val: 0.0,
            })
        }
    }
}

struct GlObjectBuffer {
    texture_id: Option<TextureId>,
    vertices: VertexBuffer<Vertex>,
    normals: VertexBuffer<Normal>,
    indices: IndexBuffer<u32>,
    wireframe_indices: Option<IndexBuffer<u32>>,
    tint_val: f32,
}

struct GlObjectBuffers {
    obj_id: ObjectId,
    buffers: Vec<GlObjectBuffer>,
}

impl GlObjectBuffers {
    fn new(display: &Display, object: &SubObject, num_textures: usize) -> Self {
        let mut textures = Vec::from_iter(std::iter::repeat_with(GlObjectsBuilder::default).take(num_textures));

        let bsp_data = &object.bsp_data;

        for (_, poly_list) in bsp_data.collision_tree.leaves() {
            for poly in poly_list {
                textures[poly.texture.0 as usize].push(bsp_data, poly);
            }
        }

        let mut buffers = vec![];
        for (i, builder) in textures.into_iter().enumerate() {
            builder.finish(display, object, Some(TextureId(i as u32)), &mut buffers)
        }
        Self { obj_id: object.obj_id, buffers }
    }
}

#[derive(Copy, Clone)]
pub struct InstanceMatrix {
    world_matrix: [[f32; 4]; 4],
}

glium::implement_vertex!(InstanceMatrix, world_matrix);

#[derive(Copy, Clone, Debug)]
pub struct Vertex {
    position: (f32, f32, f32),
    uv: (f32, f32),
}

glium::implement_vertex!(Vertex, position, uv);

#[derive(Copy, Clone)]
pub struct Normal {
    normal: (f32, f32, f32),
}

glium::implement_vertex!(Normal, normal);

impl PofToolsGui {
    fn save_model(model: &Model) -> Option<String> {
        let mut out = None;
        // use a scoped thread here, its ok to block the main window for now i guess
        crossbeam::thread::scope(|s| {
            s.spawn(|_| {
                let path = FileDialog::new()
                    .set_filename(&model.path_to_file.file_name().unwrap_or_default().to_string_lossy())
                    .add_filter("All Supported Files", &["pof", "dae"])
                    .add_filter("Parallax Object File", &["pof"])
                    .add_filter("Digital Asset Exchange file", &["dae"])
                    .show_save_single_file();
                if let Ok(Some(path)) = path {
                    let mut file = File::create(path.clone()).unwrap();
                    if path.extension().map_or(false, |s| s == "dae") {
                        model.write_dae(&mut file).unwrap();
                    } else {
                        model.write(&mut file).unwrap();
                    }
                    out = Some(path.file_name().and_then(|f| f.to_str()).unwrap_or("").to_string());
                }
            });
        })
        .unwrap();
        out
    }

    // opens a thread which opens the dialog and starts parsing a model
    fn start_loading_model(&mut self, filepath: Option<PathBuf>) {
        let (sender, receiver) = std::sync::mpsc::channel();
        self.model_loading_thread = Some(receiver);

        // the model loading thread
        std::thread::spawn(move || {
            let model = std::panic::catch_unwind(move || {
                let path = filepath.or_else(|| {
                    FileDialog::new()
                        .add_filter("All supported files", &["pof", "dae"])
                        .add_filter("COLLADA", &["dae"])
                        .add_filter("Parallax Object File", &["pof"])
                        .show_open_single_file()
                        .unwrap()
                });

                path.map(|path| {
                    let ext = path.extension().map(|ext| ext.to_ascii_lowercase());
                    let filename = path.file_name().and_then(|f| f.to_str()).unwrap_or("").to_string();
                    info!("Attempting to load {}", filename);
                    match ext.as_ref().and_then(|ext| ext.to_str()) {
                        Some("dae") => pof::parse_dae(path),
                        Some("pof") => {
                            let file = File::open(&path).expect("TODO invalid file or smth i dunno");
                            let mut parser = Parser::new(file).expect("TODO invalid version of file or smth i dunno");
                            Box::new(parser.parse(path).expect("TODO invalid pof file or smth i dunno"))
                        }
                        _ => todo!(),
                    }
                })
            });
            let _ = sender.send(model.map_err(|panic| *panic.downcast().unwrap()));
        });
    }

    fn handle_model_loading_thread(&mut self, display: &Display) {
        if let Some(thread) = &self.model_loading_thread {
            let response = thread.try_recv();
            match response {
                Ok(Ok(Some(data))) => {
                    self.model = data;
                    self.finish_loading_model(display);

                    self.model_loading_thread = None;
                }
                Err(TryRecvError::Disconnected) => self.model_loading_thread = None,
                Ok(Ok(None)) => self.model_loading_thread = None,
                Ok(Err(_)) => self.model_loading_thread = None,

                Err(TryRecvError::Empty) => {}
            }
        }
    }

    // after the above thread has returned, stuffs the new model in
    fn finish_loading_model(&mut self, display: &Display) {
        self.buffer_objects.clear();
        self.buffer_shield = None;
        self.buffer_insignias.clear();

        for subobject in &self.model.sub_objects {
            self.buffer_objects
                .push(GlObjectBuffers::new(display, subobject, self.model.textures.len()));
        }

        for insignia in &self.model.insignias {
            self.buffer_insignias.push(GlBufferedInsignia::new(display, insignia));
        }

        if let Some(shield) = &self.model.shield_data {
            self.buffer_shield = Some(GlBufferedShield::new(display, shield));
        }

        self.maybe_recalculate_3d_helpers(display);

        self.warnings.clear();
        PofToolsGui::recheck_warnings(&mut self.warnings, &self.model, All);
        PofToolsGui::recheck_errors(&mut self.errors, &self.model, All);
        self.ui_state.tree_view_selection = Default::default();
        self.ui_state.refresh_properties_panel(&self.model);
        self.camera_heading = 2.7;
        self.camera_pitch = -0.4;
        self.camera_offset = Vec3d::ZERO;
        self.camera_scale = self.model.header.max_radius * 2.0;
        self.ui_state.last_selected_subobj = self.model.header.detail_levels.first().copied();

        self.load_textures();

        let filename = self.model.path_to_file.file_name().unwrap_or_default().to_string_lossy();
        display
            .gl_window()
            .window()
            .set_title(&format!("Pof Tools v{} - {}", POF_TOOLS_VERSION, filename));

        info!("Loaded {}", filename);
    }

    fn handle_texture_loading_thread(&mut self, display: &Display) {
        if let Some(thread) = &self.texture_loading_thread {
            let response = thread.try_recv();
            match response {
                Ok(Some(data)) => {
                    let texture = SrgbTexture2d::new(display, data.0).unwrap();
                    self.buffer_textures.insert(data.1, texture);
                }
                Err(TryRecvError::Disconnected) | Ok(None) => self.texture_loading_thread = None,
                Err(TryRecvError::Empty) => {}
            }
        }
    }

    fn load_textures(&mut self) {
        self.buffer_textures.clear();
        let (sender, receiver) = std::sync::mpsc::channel();
        self.texture_loading_thread = Some(receiver);
        let textures = self.model.textures.clone();
        let path = self.model.path_to_file.clone();

        // the texture loading thread
        std::thread::spawn(move || {
            for (i, tex_name) in textures.iter().enumerate() {
                if let Some((mut file, format)) = ["png", "dds"].iter().find_map(|ext| {
                    if let Ok(file) = std::fs::File::open(path.with_file_name(format!("{}.{}", tex_name, ext))) {
                        Some((file, image::ImageFormat::from_extension(ext).unwrap()))
                    } else {
                        Some((
                            std::fs::File::open(path.with_file_name(format!("../maps/{}.{}", tex_name, ext))).ok()?,
                            image::ImageFormat::from_extension(ext).unwrap(),
                        ))
                    }
                }) {
                    let mut buf = vec![];
                    if let Err(e) = file.read_to_end(&mut buf) {
                        error!("Failed to load texture {}.{}: {:?}", tex_name, format.extensions_str()[0], e);
                        continue;
                    }
                    let image = match image::load(Cursor::new(buf), format) {
                        Ok(image) => image.to_rgba8(),
                        Err(e) => {
                            error!("Failed to load texture {}.{}: {:?}", tex_name, format.extensions_str()[0], e);
                            continue;
                        }
                    };

                    let image_dimensions = image.dimensions();
                    let image = glium::texture::RawImage2d::from_raw_rgba(image.into_raw(), image_dimensions);

                    info!("Loaded texture {}.{}", tex_name, format.extensions_str()[0]);

                    let _ = sender.send(Some((image, TextureId(i as u32))));
                }
            }

            let _ = sender.send(None);
        });
    }

    pub fn rebuild_subobj_buffers(&mut self, display: &Display, ids: Vec<ObjectId>) {
        for buf in &mut self.buffer_objects {
            if ids.contains(&buf.obj_id) {
                *buf = GlObjectBuffers::new(display, &self.model.sub_objects[buf.obj_id], self.model.textures.len());
            }
        }
    }
}

const POF_TOOLS_VERSION: &str = env!("CARGO_PKG_VERSION");

fn main() {
    // set up a panic handler to grab the backtrace
    let default_hook = std::panic::take_hook();
    let (panic_data_send, panic_data_recv) = std::sync::mpsc::sync_channel(1);
    std::panic::set_hook(Box::new(move |panic_info| {
        default_hook(panic_info);
        let backtrace = backtrace::Backtrace::new();
        let msg = panic_info.payload().downcast_ref::<String>().map_or("unknown panic", |x| x);
        let msg = format!("{},  {}", msg, panic_info.location().unwrap());
        error!("{}", msg);
        let mut frames = vec![];
        for frame in backtrace.frames() {
            // filter out anything which doesn't have pof-tools in the path
            // maybe not great? but filters out a huge amount of unrelated shit
            let should_print = frame.symbols().iter().any(|symbol| match symbol.filename().and_then(|s| s.to_str()) {
                Some(file) => file.contains("pof-tools"),
                None => symbol.name().and_then(|name| name.as_str()).map_or(false, |name| name.contains("pof")),
            });
            if should_print {
                frames.push(frame.clone())
            }
        }
        let _ = panic_data_send.send((msg, if frames.is_empty() { backtrace } else { frames.into() }));
    }));

    // set up the logger
    let mut logger_config = ConfigBuilder::new();
    logger_config.set_time_level(LevelFilter::Off);
    logger_config.set_max_level(LevelFilter::Off);
    if let Ok(file) = File::create("pof-tools.log") {
        let _ = WriteLogger::init(LevelFilter::Info, logger_config.build(), file);
    }
    info!("Pof Tools {} - {}", POF_TOOLS_VERSION, chrono::Local::now());

    let event_loop = glutin::event_loop::EventLoop::with_user_event();
    let mut pt_gui = PofToolsGui::new();
    let display = create_display(&event_loop);

    // this creates the raw bytes from png, how i make the icon
    // File::create("icon.raw")
    //     .unwrap()
    //     .write_all(&image::open("icon.png").unwrap().into_rgba8().to_vec());

    let mut args = std::env::args();
    args.next();
    let path = args.next().map(|arg| PathBuf::from(arg.as_str()));

    let mut egui = egui_glium::EguiGlium::new(&display);

    pt_gui.start_loading_model(path);

    let model = &pt_gui.model;

    // lots of graphics stuff to initialize
    let default_material_shader = glium::Program::from_source(&display, DEFAULT_VERTEX_SHADER, DEFAULT_MAT_FRAGMENT_SHADER, None).unwrap();
    let textured_material_shader = glium::Program::from_source(&display, DEFAULT_VERTEX_SHADER, TEXTURED_FRAGMENT_SHADER, None).unwrap();
    let shield_shader = glium::Program::from_source(&display, DEFAULT_VERTEX_SHADER, SHIELD_FRAGMENT_SHADER, None).unwrap();
    let wireframe_shader = glium::Program::from_source(&display, NO_NORMS_VERTEX_SHADER, WIRE_FRAGMENT_SHADER, None).unwrap();
    let lollipop_stick_shader = glium::Program::from_source(&display, NO_NORMS_VERTEX_SHADER, LOLLIPOP_STICK_FRAGMENT_SHADER, None).unwrap();
    let lollipop_shader = glium::Program::from_source(&display, LOLLIPOP_VERTEX_SHADER, LOLLIPOP_FRAGMENT_SHADER, None).unwrap();

    let mut default_material_draw_params = glium::DrawParameters {
        depth: glium::Depth {
            test: glium::draw_parameters::DepthTest::IfLess,
            write: true,
            ..Default::default()
        },
        line_width: Some(1.0),
        backface_culling: glium::draw_parameters::BackfaceCullingMode::CullCounterClockwise,
        ..Default::default()
    };
    let shield_draw_params = glium::DrawParameters {
        depth: glium::Depth {
            test: glium::draw_parameters::DepthTest::IfLess,
            write: true,
            ..Default::default()
        },
        blend: glium::Blend::alpha_blending(),
        backface_culling: glium::draw_parameters::BackfaceCullingMode::CullCounterClockwise,
        ..Default::default()
    };
    let wireframe_params = glium::DrawParameters {
        depth: glium::Depth {
            test: glium::draw_parameters::DepthTest::Overwrite,
            ..Default::default()
        },
        line_width: Some(1.0),
        backface_culling: glium::draw_parameters::BackfaceCullingMode::CullingDisabled,
        ..Default::default()
    };
    let mut lollipop_params = lollipop_params();
    let lollipop_stick_params = lollipop_stick_params();
    let lollipop_rev_depth_params = lollipop_rev_depth_params();

    let circle_verts = glium::VertexBuffer::new(&display, &*primitives::CIRCLE_VERTS).unwrap();
    let circle_indices = glium::IndexBuffer::new(&display, glium::index::PrimitiveType::LineLoop, &primitives::CIRCLE_INDICES).unwrap();

    let box_verts = glium::VertexBuffer::new(&display, &primitives::BOX_VERTS).unwrap();
    let box_indices = glium::IndexBuffer::new(&display, glium::index::PrimitiveType::LinesList, &primitives::BOX_INDICES).unwrap();

    let icosphere_verts = glium::VertexBuffer::new(&display, &primitives::SPHERE_VERTS).unwrap();
    let icosphere_indices = glium::IndexBuffer::new(&display, glium::index::PrimitiveType::TrianglesList, &primitives::SPHERE_INDICES).unwrap();

    pt_gui.camera_heading = 2.7;
    pt_gui.camera_pitch = -0.4;
    pt_gui.camera_offset = Vec3d::ZERO;
    pt_gui.camera_scale = model.header.max_radius * 2.0;

    let mut errored = None;
    info!("Beginning event loop...");

    event_loop.run(move |event, _, control_flow| {
        let mut catch_redraw = || {
            let redraw = || {
                // handle whether the thread which handles loading has responded (if it exists)
                pt_gui.handle_model_loading_thread(&display);

                pt_gui.handle_texture_loading_thread(&display);

                let needs_repaint = egui.run(&display, |ctx| pt_gui.show_ui(ctx, &display));

                *control_flow = if needs_repaint {
                    display.gl_window().window().request_redraw();
                    glutin::event_loop::ControlFlow::Poll
                } else {
                    let next_frame_time = std::time::Instant::now() + std::time::Duration::from_nanos(16_666_667);
                    glutin::event_loop::ControlFlow::WaitUntil(next_frame_time)
                };

                {
                    use glium::Surface as _;
                    let mut target = display.draw();

                    target.clear_color_and_depth((0.0, 0.0, 0.0, 1.0), 1.0);

                    // maybe redo lollipops and stuff
                    pt_gui.maybe_recalculate_3d_helpers(&display);

                    let model = &pt_gui.model;

                    let mut view_mat = glm::rotation(pt_gui.camera_pitch, &glm::vec3(1., 0., 0.)); // pitch
                    view_mat *= glm::rotation(pt_gui.camera_heading, &glm::vec3(0., 1., 0.)); // heading

                    // handle user interactions like rotating the camera
                    let rect = egui.egui_ctx.available_rect(); // the rectangle not covered by egui UI, i.e. the 3d viewport
                    if rect.is_positive() {
                        let input = egui.egui_ctx.input();
                        let mouse_pos = input.pointer.hover_pos();
                        let last_click_pos = input.pointer.press_origin();
                        let in_3d_viewport = mouse_pos.map_or(false, |hover_pos| rect.contains(hover_pos));
                        let clicked_in_3d_viewport = last_click_pos.map_or(false, |hover_pos| rect.contains(hover_pos));
                        if clicked_in_3d_viewport {
                            if !input.modifiers.shift && input.pointer.button_down(egui::PointerButton::Secondary) {
                                pt_gui.camera_heading += input.pointer.delta().x * -0.01;
                                pt_gui.camera_pitch += input.pointer.delta().y * -0.01;
                            }
                            if input.pointer.button_down(egui::PointerButton::Middle)
                                || input.modifiers.shift && input.pointer.button_down(egui::PointerButton::Secondary)
                            {
                                let x = input.pointer.delta().x * -0.005 * pt_gui.camera_scale; // for some reason x gets inverted
                                let y = input.pointer.delta().y * 0.005 * pt_gui.camera_scale;

                                pt_gui.camera_offset += view_mat.transpose().transform_vector(&glm::vec3(x, y, 0.)).into();
                            }
                        }
                        if in_3d_viewport {
                            pt_gui.camera_scale *= 1.0 + (input.scroll_delta.y * -0.001)
                        }
                    }

                    view_mat.append_translation_mut(&glm::vec3(0.0, 0.0, pt_gui.camera_scale));

                    view_mat.prepend_translation_mut(&glm::vec3(-pt_gui.camera_offset.x, -pt_gui.camera_offset.y, -pt_gui.camera_offset.z));
                    view_mat.prepend_translation_mut(&glm::vec3(-model.visual_center.x, -model.visual_center.y, -model.visual_center.z));

                    let displayed_subobjects =
                        get_list_of_display_subobjects(model, pt_gui.ui_state.tree_view_selection, pt_gui.ui_state.last_selected_subobj);

                    // brighten up the dark bits so wireframe is easier to see
                    let mut dark_color;
                    if pt_gui.display_mode == DisplayMode::Wireframe {
                        default_material_draw_params.polygon_mode = glium::draw_parameters::PolygonMode::Line;
                        default_material_draw_params.backface_culling = glium::draw_parameters::BackfaceCullingMode::CullingDisabled;
                        dark_color = [0.2, 0.2, 0.2f32];
                    } else {
                        default_material_draw_params.polygon_mode = glium::draw_parameters::PolygonMode::Fill;
                        default_material_draw_params.backface_culling = glium::draw_parameters::BackfaceCullingMode::CullCounterClockwise;
                        dark_color = [0.01, 0.01, 0.01f32];
                    }

                    // dim down the bright bits when lollipops are on screen
                    let light_color;
                    match &pt_gui.ui_state.tree_view_selection {
                        TreeSelection::Thrusters(_)
                        | TreeSelection::Weapons(_)
                        | TreeSelection::DockingBays(_)
                        | TreeSelection::Glows(_)
                        | TreeSelection::SpecialPoints(_)
                        | TreeSelection::Turrets(_)
                        | TreeSelection::Paths(_)
                        | TreeSelection::EyePoints(_)
                        | TreeSelection::VisualCenter => {
                            light_color = [0.3, 0.3, 0.3f32];
                            if pt_gui.display_mode == DisplayMode::Wireframe {
                                dark_color = [0.05, 0.05, 0.05f32];
                            }
                        }
                        _ => light_color = [1.0, 1.0, 1.0f32],
                    }

                    // set up the camera matrix
                    let perspective_matrix = {
                        let (width, height) = target.get_dimensions();
                        let aspect_ratio = height as f32 / width as f32;

                        let fov: f32 = std::f32::consts::PI / 3.0;
                        let zfar = (model.header.max_radius + pt_gui.camera_scale) * 2.0;
                        let znear = (model.header.max_radius + pt_gui.camera_scale) / 1000.;

                        let f = 1.0 / (fov / 2.0).tan();

                        Mat4x4::from([
                            [f * aspect_ratio, 0.0, 0.0, 0.0],
                            [0.0, f, 0.0, 0.0],
                            [0.0, 0.0, (zfar + znear) / (zfar - znear), 1.0],
                            [0.0, 0.0, -(2.0 * zfar * znear) / (zfar - znear), 0.0],
                        ])
                    };

                    let light_vec = glm::vec3(0.5, 1.0, -1.0);

                    // draw the actual subobjects of the model
                    for buffer_objs in &pt_gui.buffer_objects {
                        // only render if its currently being displayed
                        if displayed_subobjects[buffer_objs.obj_id] {
                            let mut mat = glm::identity::<f32, 4>();
                            mat.append_translation_mut(&pt_gui.model.get_total_subobj_offset(buffer_objs.obj_id).into());

                            let matrix = view_mat * mat;
                            let norm_matrix: [[f32; 3]; 3] = glm::mat4_to_mat3(&matrix).try_inverse().unwrap().transpose().into();
                            let vert_matrix: [[f32; 4]; 4] = (perspective_matrix * matrix).into();

                            for buffer_obj in &buffer_objs.buffers {
                                let indices = if pt_gui.display_mode == DisplayMode::Wireframe {
                                    buffer_obj.wireframe_indices.as_ref().unwrap_or(&buffer_obj.indices)
                                } else {
                                    &buffer_obj.indices
                                };

                                if let Some(texture) = buffer_obj
                                    .texture_id
                                    // if the buffer has a tex id assigned...
                                    .filter(|_| pt_gui.display_mode == DisplayMode::Textured)
                                    // if we're displaying textures...
                                    .and_then(|tex_id| pt_gui.buffer_textures.get(&tex_id))
                                //     and we have a texture loaded, then display
                                {
                                    // draw textured
                                    let uniforms = glium::uniform! {
                                        norm_matrix: norm_matrix,
                                        vert_matrix: vert_matrix,
                                        u_light: <[f32; 3]>::from(light_vec),
                                        dark_color: dark_color,
                                        light_color: light_color,
                                        tint_color: [0.0, 0.0, 1.0f32],
                                        tint_val: buffer_obj.tint_val,
                                        tex: texture,
                                    };

                                    target
                                        .draw(
                                            (&buffer_obj.vertices, &buffer_obj.normals),
                                            indices,
                                            &textured_material_shader,
                                            &uniforms,
                                            &default_material_draw_params,
                                        )
                                        .unwrap();
                                } else {
                                    // draw untextured
                                    let uniforms = glium::uniform! {
                                        norm_matrix: norm_matrix,
                                        vert_matrix: vert_matrix,
                                        u_light: <[f32; 3]>::from(light_vec),
                                        dark_color: dark_color,
                                        light_color: light_color,
                                        tint_color: [0.0, 0.0, 1.0f32],
                                        tint_val: buffer_obj.tint_val,
                                    };

                                    target
                                        .draw(
                                            (&buffer_obj.vertices, &buffer_obj.normals),
                                            indices,
                                            &default_material_shader,
                                            &uniforms,
                                            &default_material_draw_params,
                                        )
                                        .unwrap();
                                }
                            }
                        }
                    }

                    // maybe draw the insignias
                    if let TreeSelection::Insignia(insignia_select) = pt_gui.tree_view_selection {
                        let (current_detail_level, current_insignia_idx) = match insignia_select {
                            InsigniaSelection::Header => (0, None),
                            InsigniaSelection::Insignia(idx) => (pt_gui.model.insignias[idx].detail_level, Some(idx)),
                        };

                        for (i, insig_buffer) in pt_gui.buffer_insignias.iter().enumerate() {
                            let insignia = &pt_gui.model.insignias[i];
                            if insignia.detail_level == current_detail_level {
                                let mut mat = glm::identity::<f32, 4>();
                                mat.append_translation_mut(&insignia.offset.into());
                                let matrix = view_mat * mat;
                                let norm_matrix: [[f32; 3]; 3] = glm::mat4_to_mat3(&matrix).try_inverse().unwrap().transpose().into();
                                let vert_matrix: [[f32; 4]; 4] = (perspective_matrix * matrix).into();

                                let color = if current_insignia_idx == Some(i) {
                                    [1.0, 0.0, 0.0f32]
                                } else {
                                    [0.0, 0.0, 1.0f32]
                                };

                                // only render if its currently being displayed
                                let uniforms = glium::uniform! {
                                    norm_matrix: norm_matrix,
                                    vert_matrix: vert_matrix,
                                    u_light: <[f32; 3]>::from(light_vec),
                                    dark_color: dark_color,
                                    light_color: light_color,
                                    tint_color: color,
                                    tint_val: 0.3f32,
                                };

                                target
                                    .draw(
                                        (&insig_buffer.vertices, &insig_buffer.normals),
                                        &insig_buffer.indices,
                                        &default_material_shader,
                                        &uniforms,
                                        &default_material_draw_params,
                                    )
                                    .unwrap();
                            }
                        }
                    }

                    // maybe draw the shield
                    if let TreeSelection::Shield = pt_gui.tree_view_selection {
                        if let Some(shield) = &pt_gui.buffer_shield {
                            let matrix = view_mat;
                            let norm_matrix: [[f32; 3]; 3] = glm::mat4_to_mat3(&matrix).try_inverse().unwrap().transpose().into();
                            let vert_matrix: [[f32; 4]; 4] = (perspective_matrix * matrix).into();

                            let uniforms = glium::uniform! {
                                norm_matrix: norm_matrix,
                                vert_matrix: vert_matrix,
                                u_light: [0.0, 0.0, -1.0f32],
                                dark_color: [0.0, 0.0, 0.0f32],
                                light_color: [0.2, 0.3, 0.9f32],
                                tint_color: [0.0, 0.0, 1.0f32],
                                tint_val: 0.0f32,
                            };

                            target
                                .draw((&shield.vertices, &shield.normals), &shield.indices, &shield_shader, &uniforms, &shield_draw_params)
                                .unwrap();

                            //      DEBUG - Draw BSP node bounding boxes
                            //      This is quite useful but incredibly ineffcient
                            //      TODO make this more efficient
                            //
                            // let initial_node = pt_gui.model.shield_data.as_ref().unwrap().collision_tree.as_ref().unwrap();
                            // let mut node_stack = vec![(initial_node, 0u32)];
                            // while let Some((node, depth)) = node_stack.pop() {
                            //     let bbox = match node {
                            //         pof::ShieldNode::Split { bbox, front, back, .. } => {
                            //             node_stack.push((front, depth + 1));
                            //             node_stack.push((back, depth + 1));
                            //             bbox
                            //         }
                            //         pof::ShieldNode::Leaf { bbox, .. } => bbox,
                            //     };

                            //     let mut mat = glm::scaling(&(bbox.max - bbox.min).into());
                            //     mat.append_translation_mut(&(bbox.min).into());
                            //     let color = 2.0 / (1.5f32.powf(depth as f32));

                            //     let uniforms = glium::uniform! {
                            //         model: <[[f32; 4]; 4]>::from(mat),
                            //         view: view_mat,
                            //         perspective: perspective_matrix,
                            //         lollipop_color: [color, color, color, 1.0f32],
                            //     };

                            //     target
                            //         .draw(&box_verts, &box_indices, &lollipop_stick_shader, &uniforms, &lollipop_stick_params)
                            //         .unwrap();
                            // }
                        }
                    }

                    let mut obj_id = None; // None also indicates the header being possibly selected
                    if let TreeSelection::SubObjects(SubObjectSelection::SubObject(id)) = pt_gui.tree_view_selection {
                        obj_id = Some(id);

                        //      DEBUG - Draw BSP node bounding boxes
                        //      This is quite useful but incredibly ineffcient
                        //      TODO make this more efficient
                        //
                        let mut node_stack = vec![(&pt_gui.model.sub_objects[id].bsp_data.collision_tree, 0u32)];
                        while let Some((node, depth)) = node_stack.pop() {
                            let bbox = match node {
                                BspNode::Split { bbox, front, back, .. } => {
                                    node_stack.push((front, depth + 1));
                                    node_stack.push((back, depth + 1));
                                    bbox
                                }
                                BspNode::Leaf { bbox, .. } => bbox,
                            };

                            let mut mat = glm::scaling(&(bbox.max - bbox.min).into());
                            mat.append_translation_mut(&(bbox.min + pt_gui.model.get_total_subobj_offset(id)).into());
                            let color = 2.0 / (1.5f32.powf(depth as f32));
                            let matrix = view_mat * mat;
                            let vert_matrix: [[f32; 4]; 4] = (perspective_matrix * matrix).into();

                            let uniforms = glium::uniform! {
                                vert_matrix: vert_matrix,
                                lollipop_color: [color, color, color, 1.0f32],
                            };

                            target
                                .draw(&box_verts, &box_indices, &lollipop_stick_shader, &uniforms, &lollipop_stick_params)
                                .unwrap();
                        }
                    }

                    // draw wireframe bounding boxes
                    if pt_gui.display_bbox {
                        let bbox = if let Some(id) = obj_id {
                            &pt_gui.model.sub_objects[id].bbox
                        } else {
                            &pt_gui.model.header.bbox
                        };

                        let mut mat = glm::scaling(&(bbox.max - bbox.min).into());
                        let offset = if let Some(id) = obj_id {
                            pt_gui.model.get_total_subobj_offset(id)
                        } else {
                            Vec3d::ZERO
                        };
                        mat.append_translation_mut(&(bbox.min + offset).into());

                        let matrix = view_mat * mat;
                        let vert_matrix: [[f32; 4]; 4] = (perspective_matrix * matrix).into();

                        let uniforms = glium::uniform! {
                            vert_matrix: vert_matrix
                        };

                        target
                            .draw(&box_verts, &box_indices, &wireframe_shader, &uniforms, &wireframe_params)
                            .unwrap();
                    }

                    // draw wireframe 'sphere'
                    if pt_gui.display_radius {
                        for i in 0..3 {
                            let rad = if let Some(id) = obj_id {
                                pt_gui.model.sub_objects[id].radius
                            } else {
                                pt_gui.model.header.max_radius
                            };

                            let mut mat = glm::scaling(&glm::vec3(rad, rad, rad));
                            if i == 1 {
                                mat *= glm::rotation(std::f32::consts::FRAC_PI_2, &glm::vec3(0.0, 1.0, 0.0));
                            } else if i == 2 {
                                mat *= glm::rotation(std::f32::consts::FRAC_PI_2, &glm::vec3(1.0, 0.0, 0.0));
                            }

                            let offset = if let Some(id) = obj_id {
                                pt_gui.model.get_total_subobj_offset(id)
                            } else {
                                Vec3d::ZERO
                            };
                            mat.append_translation_mut(&offset.into());
                            let matrix = view_mat * mat;
                            let vert_matrix: [[f32; 4]; 4] = (perspective_matrix * matrix).into();

                            let uniforms = glium::uniform! {
                                vert_matrix: vert_matrix
                            };

                            target
                                .draw(&circle_verts, &circle_indices, &wireframe_shader, &uniforms, &wireframe_params)
                                .unwrap();
                        }
                    }

                    // don't display lollipops if you're in header or subobjects, unless display_origin is on, since that's the only lollipop they have
                    let display_lollipops = (!matches!(pt_gui.ui_state.tree_view_selection, TreeSelection::Header)
                        && !matches!(pt_gui.ui_state.tree_view_selection, TreeSelection::SubObjects(_)))
                        || pt_gui.display_origin;

                    if display_lollipops {
                        for lollipop_group in &pt_gui.lollipops {
                            if let TreeSelection::Paths(_) = pt_gui.ui_state.tree_view_selection {
                                lollipop_params.blend = glium::Blend::alpha_blending();
                            } else {
                                lollipop_params.blend = ADDITIVE_BLEND;
                            }
                            let vert_matrix: [[f32; 4]; 4] = (perspective_matrix * view_mat).into();

                            let uniforms = glium::uniform! {
                                vert_matrix: vert_matrix,
                                lollipop_color: lollipop_group.color,
                            };

                            target
                                .draw(
                                    (&icosphere_verts, lollipop_group.lolly_vertices.per_instance().unwrap()),
                                    &icosphere_indices,
                                    &lollipop_shader,
                                    &uniforms,
                                    &lollipop_params,
                                )
                                .unwrap();
                            target
                                .draw(
                                    &lollipop_group.stick_vertices,
                                    &lollipop_group.stick_indices,
                                    &lollipop_stick_shader,
                                    &uniforms,
                                    &lollipop_stick_params,
                                )
                                .unwrap();

                            if !matches!(pt_gui.ui_state.tree_view_selection, TreeSelection::Paths(_)) {
                                // ...then draw the lollipops with reverse depth order, darker
                                // this gives the impression that the lollipops are dimly visible through the model
                                // (dont do it for path lollipops, they're busy enough)

                                // same uniforms as above, but darkened
                                // i cant just modify the previous uniforms variable, the uniforms! macro is doing some crazy shit
                                let vert_matrix: [[f32; 4]; 4] = (perspective_matrix * view_mat).into();

                                let uniforms = glium::uniform! {
                                    vert_matrix: vert_matrix,
                                    lollipop_color: lollipop_group.color.map(|col| col * 0.15 ), // <<< THIS IS DIFFERENT FROM ABOVE
                                };

                                target
                                    .draw(
                                        (&icosphere_verts, lollipop_group.lolly_vertices.per_instance().unwrap()),
                                        &icosphere_indices,
                                        &lollipop_shader,
                                        &uniforms,
                                        &lollipop_rev_depth_params,
                                    )
                                    .unwrap();
                            }
                        }
                    }

                    egui.paint(&display, &mut target);

                    target.finish().unwrap();
                }
            };

            // error handling (crude as it is)
            // do the whole frame, catch any panics
            if errored.is_none() {
                drop(std::panic::catch_unwind(std::panic::AssertUnwindSafe(redraw)));
                match panic_data_recv.try_recv() {
                    Ok((msg, backtrace)) => errored = Some((msg, backtrace)),
                    Err(e) => match e {
                        TryRecvError::Empty => {}
                        TryRecvError::Disconnected => errored = Some(("double panic".into(), backtrace::Backtrace::new())),
                    },
                }
            }

            // if there was an error, do this mini event loop and display the error message
            if let Some((error_string, backtrace)) = &errored {
                let needs_repaint = egui.run(&display, |ctx| {
                    egui::CentralPanel::default().show(ctx, |ui| {
                        egui::ScrollArea::vertical().auto_shrink([false, false]).show(ui, |ui| {
                            ui.horizontal(|ui| {
                                ui.heading(RichText::new("Error! Please report this!").color(Color32::RED));
                                if ui.button("Copy").clicked() {
                                    ui.output().copied_text = format!("{}\n\n{:?}", error_string, backtrace);
                                }
                            });
                            ui.add_sized(ui.available_size(), TextEdit::multiline(&mut &*format!("{}\n\n{:?}", error_string, backtrace)));
                        });
                    });
                });

                *control_flow = if needs_repaint {
                    display.gl_window().window().request_redraw();
                    glutin::event_loop::ControlFlow::Poll
                } else {
                    let next_frame_time = std::time::Instant::now() + std::time::Duration::from_nanos(16_666_667);
                    glutin::event_loop::ControlFlow::WaitUntil(next_frame_time)
                };
                let mut target = display.draw();
                use glium::Surface as _;
                target.clear_color_and_depth((0.0, 0.0, 0.0, 1.0), 1.0);

                egui.paint(&display, &mut target);

                target.finish().unwrap();
            }
        };

        match event {
            // Platform-dependent event handlers to workaround a winit bug
            // See: https://github.com/rust-windowing/winit/issues/987
            // See: https://github.com/rust-windowing/winit/issues/1619
            glutin::event::Event::RedrawEventsCleared if cfg!(windows) => catch_redraw(),
            glutin::event::Event::RedrawRequested(_) if !cfg!(windows) => catch_redraw(),
            glutin::event::Event::DeviceEvent { .. } => {
                // match event {
                //     glutin::event::DeviceEvent::Added => todo!(),
                //     glutin::event::DeviceEvent::Removed => todo!(),
                //     glutin::event::DeviceEvent::MouseMotion { delta } => todo!(),
                //     glutin::event::DeviceEvent::MouseWheel { delta } => todo!(),
                //     glutin::event::DeviceEvent::Motion { axis, value } => todo!(),
                //     glutin::event::DeviceEvent::Button { button, state } => todo!(),
                //     glutin::event::DeviceEvent::Key(_) => todo!(),
                //     glutin::event::DeviceEvent::Text { codepoint } => todo!(),
                // }
            }

            glutin::event::Event::WindowEvent { event, .. } => {
                if let WindowEvent::CloseRequested = event {
                    *control_flow = glium::glutin::event_loop::ControlFlow::Exit;
                }

                egui.on_event(&event);

                display.gl_window().window().request_redraw(); // TODO: ask egui if the events warrants a repaint instead
            }

            _ => (),
        };
    });
}

// based on the current selection which submodels should be displayed
// TODO show destroyed models
fn get_list_of_display_subobjects(model: &Model, tree_selection: TreeSelection, last_selected_subobj: Option<ObjectId>) -> ObjVec<bool> {
    let mut out = ObjVec(vec![false; model.sub_objects.len()]);

    if model.sub_objects.is_empty() {
        return out;
    }

    if let TreeSelection::Insignia(InsigniaSelection::Insignia(idx)) = tree_selection {
        // show the LOD objects according to the detail level of the currently selected insignia
        for (i, sub_object) in model.sub_objects.iter().enumerate() {
            out.0[i] = model.is_obj_id_ancestor(sub_object.obj_id, model.header.detail_levels[model.insignias[idx].detail_level as usize])
                && !sub_object.is_destroyed_model();
        }
    } else if let Some(last_selected_subobj) = last_selected_subobj {
        //find the top level parent of the currently subobject
        let mut top_level_parent = last_selected_subobj;
        while let Some(id) = model.sub_objects[top_level_parent].parent() {
            top_level_parent = id;
        }

        fn display_subobject_recursive(display_subobjects: &mut ObjVec<bool>, subobjects: &ObjVec<SubObject>, id: ObjectId) {
            display_subobjects[id] = true;

            for child_id in subobjects[id].children() {
                if !subobjects[*child_id].is_destroyed_model() {
                    display_subobject_recursive(display_subobjects, subobjects, *child_id);
                }
            }
        }

        display_subobject_recursive(&mut out, &model.sub_objects, top_level_parent);

        // if they have debris selected show all the debris
        if model.sub_objects[last_selected_subobj].is_debris_model {
            for (i, sub_object) in model.sub_objects.iter().enumerate() {
                out.0[i] = sub_object.is_debris_model;
            }
        }
    }

    out
}

impl PofToolsGui {
    fn maybe_recalculate_3d_helpers(&mut self, display: &Display) {
        if self.buffer_objects.is_empty() {
            return;
        }

        // always recalculate for glowpoint sim, TODO maybe make that a little smarter
        if !(self.ui_state.viewport_3d_dirty || (self.glow_point_simulation && matches!(self.tree_view_selection, TreeSelection::Glows(_)))) {
            return;
        }

        for buffers in &mut self.buffer_objects {
            for buffer in &mut buffers.buffers {
                buffer.tint_val = 0.0;
            }
        }

        self.lollipops.clear();

        let model = &self.model;

        const UNSELECTED: usize = 0;
        const SELECTED_POINT: usize = 1;
        const SELECTED_BANK: usize = 2;

        // determine what bank/point is selected, if any
        // push the according lollipop positions/normals based on the points positions/normals
        // push into 3 separate vectors, for 3 separate colors depending on selection state
        match self.ui_state.tree_view_selection {
            TreeSelection::SubObjects(SubObjectSelection::SubObject(obj_id)) => {
                for buffers in &mut self.buffer_objects {
                    if buffers.obj_id == obj_id {
                        for buffer in &mut buffers.buffers {
                            buffer.tint_val = 0.2;
                        }
                    }

                    let size = 0.05 * model.sub_objects[obj_id].radius;

                    let mut lollipop_origin = GlLollipopsBuilder::new(LOLLIPOP_SELECTED_POINT_COLOR);
                    lollipop_origin.push(model.get_total_subobj_offset(obj_id), Vec3d::ZERO, size);
                    let lollipop_origin = lollipop_origin.finish(display);

                    self.lollipops = vec![lollipop_origin];
                }
            }
            TreeSelection::Textures(TextureSelection::Texture(tex)) => {
                for buffers in &mut self.buffer_objects {
                    for buffer in &mut buffers.buffers {
                        if buffer.texture_id == Some(tex) {
                            buffer.tint_val = 0.3;
                        }
                    }
                }
            }
            TreeSelection::Thrusters(thruster_selection) => {
                let mut selected_bank = None;
                let mut selected_point = None;
                match thruster_selection {
                    ThrusterSelection::Bank(bank) => selected_bank = Some(bank),
                    ThrusterSelection::BankPoint(bank, point) => {
                        selected_bank = Some(bank);
                        selected_point = Some(point);
                    }
                    _ => {}
                }

                const COLORS: [[f32; 4]; 3] = [LOLLIPOP_UNSELECTED_COLOR, LOLLIPOP_SELECTED_POINT_COLOR, LOLLIPOP_SELECTED_BANK_COLOR];
                self.lollipops = build_lollipops(
                    &COLORS,
                    display,
                    model.thruster_banks.iter().enumerate().flat_map(|(bank_idx, thruster_bank)| {
                        thruster_bank.glows.iter().enumerate().map(move |(point_idx, thruster_point)| {
                            let position = thruster_point.position;
                            let normal = thruster_point.normal * thruster_point.radius * 2.0;
                            let radius = thruster_point.radius;
                            let selection = if selected_bank == Some(bank_idx) {
                                if selected_point == Some(point_idx) {
                                    SELECTED_POINT
                                } else {
                                    SELECTED_BANK
                                }
                            } else {
                                UNSELECTED
                            };
                            (position, normal, radius, selection)
                        })
                    }),
                );
            }
            TreeSelection::Weapons(weapons_selection) => {
                let mut selected_bank = None;
                let mut selected_point = None;
                let mut selected_weapon_system = None;
                match weapons_selection {
                    WeaponSelection::PriBank(bank) => {
                        selected_bank = Some(bank);
                        selected_weapon_system = Some(&model.primary_weps);
                    }
                    WeaponSelection::SecBank(bank) => {
                        selected_bank = Some(bank);
                        selected_weapon_system = Some(&model.secondary_weps);
                    }
                    WeaponSelection::PriBankPoint(bank, point) => {
                        selected_bank = Some(bank);
                        selected_point = Some(point);
                        selected_weapon_system = Some(&model.primary_weps);
                    }
                    WeaponSelection::SecBankPoint(bank, point) => {
                        selected_bank = Some(bank);
                        selected_point = Some(point);
                        selected_weapon_system = Some(&model.secondary_weps);
                    }
                    WeaponSelection::PriHeader => {
                        selected_weapon_system = Some(&model.primary_weps);
                    }
                    WeaponSelection::SecHeader => {
                        selected_weapon_system = Some(&model.secondary_weps);
                    }
                    _ => {}
                }

                let weapon_system = if let Some(weapon_list) = selected_weapon_system {
                    weapon_list.iter().collect::<Vec<_>>()
                } else {
                    model.primary_weps.iter().chain(&model.secondary_weps).collect()
                };

                const COLORS: [[f32; 4]; 3] = [LOLLIPOP_UNSELECTED_COLOR, LOLLIPOP_SELECTED_POINT_COLOR, LOLLIPOP_SELECTED_BANK_COLOR];
                self.lollipops = build_lollipops(
                    &COLORS,
                    display,
                    weapon_system.iter().enumerate().flat_map(|(bank_idx, weapon_bank)| {
                        weapon_bank.iter().enumerate().map(move |(point_idx, weapon_point)| {
                            let position = weapon_point.position;
                            let radius = model.header.max_radius * 0.03;
                            let normal = weapon_point.normal.0 * radius * 2.0;
                            let selection = if selected_bank == Some(bank_idx) {
                                if selected_point == Some(point_idx) {
                                    SELECTED_POINT
                                } else {
                                    SELECTED_BANK
                                }
                            } else {
                                UNSELECTED
                            };
                            (position, normal, radius, selection)
                        })
                    }),
                );
            }
            TreeSelection::DockingBays(docking_selection) => {
                let selected_bank = if let DockingSelection::Bay(num) = docking_selection {
                    Some(num)
                } else {
                    None
                };

                const COLORS: [[f32; 4]; 3] = [LOLLIPOP_UNSELECTED_COLOR, LOLLIPOP_SELECTED_POINT_COLOR, LOLLIPOP_SELECTED_BANK_COLOR];
                self.lollipops = build_lollipops(
                    &COLORS,
                    display,
                    model.docking_bays.iter().enumerate().flat_map(|(bay_idx, docking_bay)| {
                        let mut position = docking_bay.position;
                        if let Some(parent_name) = pof::properties_get_field(&docking_bay.properties, "$parent_submodel") {
                            if let Some(id) = model.get_obj_id_by_name(parent_name) {
                                position += model.get_total_subobj_offset(id);
                            }
                        }
                        let radius = self.model.header.max_radius.powf(0.4) / 4.0;
                        let fvec = docking_bay.fvec.0 * radius * 3.0;
                        let uvec = docking_bay.uvec.0 * radius * 3.0;
                        let (selection1, selection2) = if selected_bank == Some(bay_idx) {
                            (SELECTED_BANK, SELECTED_POINT)
                        } else {
                            (UNSELECTED, UNSELECTED)
                        };

                        let lollipop1 = (position, fvec, radius, selection1);
                        let lollipop2 = (position, uvec, 0.0, selection2);
                        vec![lollipop1, lollipop2]
                    }),
                );
            }
            TreeSelection::Glows(glow_selection) => {
                let mut selected_bank = None;
                let mut selected_point = None;
                match glow_selection {
                    GlowSelection::Bank(bank) => selected_bank = Some(bank),
                    GlowSelection::BankPoint(bank, point) => {
                        selected_bank = Some(bank);
                        selected_point = Some(point);
                    }
                    _ => {}
                }

                let elapsed = self.glow_point_sim_start.elapsed().as_millis();

                const COLORS: [[f32; 4]; 3] = [LOLLIPOP_UNSELECTED_COLOR, LOLLIPOP_SELECTED_POINT_COLOR, LOLLIPOP_SELECTED_BANK_COLOR];
                self.lollipops = build_lollipops(
                    &COLORS,
                    display,
                    model.glow_banks.iter().enumerate().flat_map(|(bank_idx, glow_bank)| {
                        let enabled = !self.glow_point_simulation
                            || (elapsed as i128 - glow_bank.disp_time as i128).rem_euclid(glow_bank.on_time as i128 + glow_bank.off_time as i128)
                                < glow_bank.on_time as i128;
                        glow_bank.glow_points.iter().enumerate().map(move |(point_idx, glow_point)| {
                            let position = glow_point.position;
                            let normal = glow_point.normal * glow_point.radius * 2.0;
                            let radius = glow_point.radius * if enabled { 1.0 } else { 0.25 };
                            let selection = if selected_bank == Some(bank_idx) {
                                if selected_point == Some(point_idx) {
                                    SELECTED_POINT
                                } else {
                                    SELECTED_BANK
                                }
                            } else {
                                UNSELECTED
                            };
                            (position, normal, radius, selection)
                        })
                    }),
                );
            }
            TreeSelection::SpecialPoints(special_selection) => {
                let mut selected_point = None;
                if let SpecialPointSelection::Point(point) = special_selection {
                    selected_point = Some(point);
                }

                const COLORS: [[f32; 4]; 2] = [LOLLIPOP_SELECTED_BANK_COLOR, LOLLIPOP_SELECTED_POINT_COLOR];
                self.lollipops = build_lollipops(
                    &COLORS,
                    display,
                    model.special_points.iter().enumerate().map(|(point_idx, special_point)| {
                        let position = special_point.position;
                        let normal = Default::default(); // 0 vec
                        let radius = special_point.radius;
                        let selection = if selected_point == Some(point_idx) { SELECTED_POINT } else { UNSELECTED };
                        (position, normal, radius, selection)
                    }),
                );
            }
            TreeSelection::Turrets(turret_selection) => {
                let mut selected_turret = None;
                let mut selected_point = None;
                match turret_selection {
                    TurretSelection::Turret(turret) => selected_turret = Some(turret),
                    TurretSelection::TurretPoint(turret, point) => {
                        selected_turret = Some(turret);
                        selected_point = Some(point);
                    }
                    _ => {}
                }

                let size = 0.007 * model.header.max_radius;

                const COLORS: [[f32; 4]; 3] = [LOLLIPOP_UNSELECTED_COLOR, LOLLIPOP_SELECTED_POINT_COLOR, LOLLIPOP_SELECTED_BANK_COLOR];
                self.lollipops = build_lollipops(
                    &COLORS,
                    display,
                    model.turrets.iter().enumerate().flat_map(|(turret_idx, turret)| {
                        let offset = self.model.get_total_subobj_offset(turret.gun_obj);
                        turret.fire_points.iter().enumerate().map(move |(point_idx, fire_point)| {
                            let position = *fire_point + offset;
                            let normal = turret.normal.0 * size * 2.0;
                            let radius = size;
                            let selection = if selected_turret == Some(turret_idx) {
                                if selected_point == Some(point_idx) {
                                    SELECTED_POINT
                                } else {
                                    SELECTED_BANK
                                }
                            } else {
                                UNSELECTED
                            };
                            (position, normal, radius, selection)
                        })
                    }),
                );
            }
            TreeSelection::Paths(path_selection) => {
                let mut selected_path = None;
                let mut selected_point = None;
                match path_selection {
                    PathSelection::Path(path) => selected_path = Some(path),
                    PathSelection::PathPoint(path, point) => {
                        selected_path = Some(path);
                        selected_point = Some(point);
                    }
                    _ => {}
                }

                const COLORS: [[f32; 4]; 3] = [
                    LOLLIPOP_UNSELECTED_PATH_COLOR,
                    LOLLIPOP_SELECTED_PATH_POINT_COLOR,
                    LOLLIPOP_SELECTED_PATH_COLOR,
                ];
                self.lollipops = build_lollipops(
                    &COLORS,
                    display,
                    model.paths.iter().enumerate().flat_map(|(path_idx, path)| {
                        path.points.iter().enumerate().map(move |(point_idx, path_point)| {
                            let position = path_point.position;
                            let radius = path_point.radius;
                            let normal = {
                                if point_idx != path.points.len() - 1 {
                                    path.points[point_idx + 1].position - path_point.position
                                } else {
                                    Default::default()
                                }
                            };
                            let selection = if selected_path == Some(path_idx) {
                                if selected_point == Some(point_idx) {
                                    SELECTED_POINT
                                } else {
                                    SELECTED_BANK
                                }
                            } else {
                                UNSELECTED
                            };
                            (position, normal, radius, selection)
                        })
                    }),
                );
            }
            TreeSelection::EyePoints(eye_selection) => {
                let mut selected_eye = None;
                if let EyeSelection::EyePoint(point) = eye_selection {
                    selected_eye = Some(point)
                }

                let size = 0.007 * model.header.max_radius;

                const COLORS: [[f32; 4]; 2] = [LOLLIPOP_SELECTED_BANK_COLOR, LOLLIPOP_SELECTED_POINT_COLOR];
                self.lollipops = build_lollipops(
                    &COLORS,
                    display,
                    model.eye_points.iter().enumerate().map(|(eye_idx, eye_point)| {
                        let position = eye_point.offset;
                        let normal = eye_point.normal.0 * size * 2.0;
                        let radius = size;
                        let selection = if selected_eye == Some(eye_idx) { SELECTED_POINT } else { UNSELECTED };
                        (position, normal, radius, selection)
                    }),
                );
            }
            TreeSelection::VisualCenter => {
                let size = 0.02 * model.header.max_radius;

                let mut lollipop_origin = GlLollipopsBuilder::new(LOLLIPOP_SELECTED_BANK_COLOR);
                lollipop_origin.push(Vec3d::ZERO, Vec3d::ZERO, size);
                let lollipop_origin = lollipop_origin.finish(display);

                let mut lollipop_visual_center = GlLollipopsBuilder::new(LOLLIPOP_SELECTED_POINT_COLOR);
                lollipop_visual_center.push(model.visual_center, Vec3d::ZERO, size);
                let lollipop_visual_center = lollipop_visual_center.finish(display);

                self.lollipops = vec![lollipop_origin, lollipop_visual_center];
            }
            _ => {}
        }

        self.ui_state.viewport_3d_dirty = false;
    }
}

const LOLLIPOP_UNSELECTED_COLOR: [f32; 4] = [0.3, 0.3, 0.3, 0.15];
const LOLLIPOP_SELECTED_BANK_COLOR: [f32; 4] = [0.15, 0.15, 1.0, 0.15];
const LOLLIPOP_SELECTED_POINT_COLOR: [f32; 4] = [1.0, 0.15, 0.15, 0.15];

const LOLLIPOP_UNSELECTED_PATH_COLOR: [f32; 4] = [0.3, 0.3, 0.3, 0.005];
const LOLLIPOP_SELECTED_PATH_COLOR: [f32; 4] = [0.15, 0.15, 1.0, 0.05];
const LOLLIPOP_SELECTED_PATH_POINT_COLOR: [f32; 4] = [1.0, 0.15, 0.15, 0.1];

fn lollipop_params() -> glium::DrawParameters<'static> {
    glium::DrawParameters {
        depth: glium::Depth {
            test: glium::draw_parameters::DepthTest::IfLess,
            ..Default::default()
        },
        blend: ADDITIVE_BLEND,
        backface_culling: glium::draw_parameters::BackfaceCullingMode::CullingDisabled,
        ..Default::default()
    }
}

fn lollipop_stick_params() -> glium::DrawParameters<'static> {
    glium::DrawParameters {
        depth: glium::Depth {
            test: glium::draw_parameters::DepthTest::IfLess,
            ..Default::default()
        },
        line_width: Some(2.0),
        backface_culling: glium::draw_parameters::BackfaceCullingMode::CullingDisabled,
        ..Default::default()
    }
}

fn lollipop_rev_depth_params() -> glium::DrawParameters<'static> {
    glium::DrawParameters {
        depth: glium::Depth {
            test: glium::draw_parameters::DepthTest::IfMore,
            ..Default::default()
        },
        blend: ADDITIVE_BLEND,
        backface_culling: glium::draw_parameters::BackfaceCullingMode::CullingDisabled,
        ..Default::default()
    }
}

const DEFAULT_VERTEX_SHADER: &str = r#"
#version 140

in vec3 position;
in vec3 normal;
in vec2 uv;

out vec2 v_uv;
out vec3 v_normal;

uniform mat4 vert_matrix;
uniform mat3 norm_matrix;

void main() {
    v_uv = uv;
    v_normal = norm_matrix * normal;
    gl_Position = vert_matrix * vec4(position, 1.0);
}
"#;

const NO_NORMS_VERTEX_SHADER: &str = r#"
#version 140

in vec3 position;

uniform mat4 vert_matrix;

void main() {
    gl_Position = vert_matrix * vec4(position, 1.0);
}
"#;

const TEXTURED_FRAGMENT_SHADER: &str = r#"
#version 140

in vec3 v_normal;
in vec2 v_uv;

out vec4 color;

uniform vec3 u_light;
uniform vec3 dark_color;
uniform vec3 light_color;
uniform vec3 tint_color;
uniform float tint_val;
uniform sampler2D tex;

void main() {
    float brightness = dot(normalize(v_normal), normalize(u_light));
    brightness = 0.3 + 0.7 * brightness;
    brightness = clamp(brightness, 0.0, 1.0);
    brightness = pow(brightness, 2.5);

    vec4 tex_color = texture(tex, v_uv);
    vec3 untinted_color = mix(dark_color * tex_color.xyz, light_color * tex_color.xyz, brightness);

    color = vec4(mix(untinted_color, tint_color, tint_val), 0.5);
}
"#;

const DEFAULT_MAT_FRAGMENT_SHADER: &str = r#"
#version 140

in vec3 v_normal;

out vec4 color;

uniform vec3 u_light;
uniform vec3 dark_color;
uniform vec3 light_color;
uniform vec3 tint_color;
uniform float tint_val;

void main() {
    float brightness = dot(normalize(v_normal), normalize(u_light));
    brightness = 0.3 + 0.7 * brightness;
    brightness = clamp(brightness, 0.0, 1.0);
    brightness = pow(brightness, 2.5);

    vec3 untinted_color = mix(dark_color, light_color, brightness);

    color = vec4(mix(untinted_color, tint_color, tint_val), 0.5);
}
"#;

const SHIELD_FRAGMENT_SHADER: &str = r#"
#version 140

in vec3 v_normal;

out vec4 color;

uniform vec3 u_light;
uniform vec3 dark_color;
uniform vec3 light_color;
uniform vec3 tint_color;
uniform float tint_val;

void main() {
    float brightness = dot(normalize(v_normal), normalize(u_light));
    brightness = 0.3 + 0.7 * brightness;
    brightness = clamp(brightness, 0.0, 1.0);
    brightness = pow(brightness, 2.5);

    vec3 untinted_color = mix(dark_color, light_color, brightness);

    color = vec4(mix(untinted_color, tint_color, tint_val), 0.5);
}
"#;

const LOLLIPOP_VERTEX_SHADER: &str = r#"
#version 140

in mat4 world_matrix;
in vec3 position;

uniform mat4 vert_matrix;

void main() {
    gl_Position = vert_matrix * world_matrix * vec4(position, 1.0);
}
"#;

const WIRE_FRAGMENT_SHADER: &str = r#"
#version 140

out vec4 color;

void main() {
    color = vec4(1.0, 1.0, 1.0, 1.0);
}
"#;

const LOLLIPOP_STICK_FRAGMENT_SHADER: &str = r#"
#version 140

out vec4 color;

uniform vec4 lollipop_color;

void main() {
    color = vec4(lollipop_color.xyz * 0.5, 1.0);
}
"#;

const LOLLIPOP_FRAGMENT_SHADER: &str = r#"
#version 140

out vec4 color;

uniform vec4 lollipop_color;

void main() {
    color = lollipop_color;
}
"#;
