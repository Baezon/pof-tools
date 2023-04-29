//     This disables the console that pops up if you run the program on windows but also supresses output on stdout
// VVV wasn't sure how to handle it so i leave it on for debug but turn it off for release
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]
#![allow(clippy::useless_format)]
#![allow(clippy::explicit_auto_deref)]
#[macro_use]
extern crate log;
extern crate nalgebra_glm as glm;
extern crate simplelog;

use crate::{
    primitives::OCTAHEDRON_VERTS,
    ui::{
        DisplayMode, DockingTreeValue, DragAxis, EyeTreeValue, GlowTreeValue, InsigniaTreeValue, PathTreeValue, SpecialPointTreeValue,
        SubObjectTreeValue, TextureTreeValue, ThrusterTreeValue, TurretTreeValue, UndoAction, WeaponTreeValue,
    },
};
use eframe::egui::PointerButton;
use egui::{Color32, RichText, TextEdit};
use glium::{
    glutin::{self, event::WindowEvent, window::Icon},
    texture::SrgbTexture2d,
    BlendingFunction, Display, IndexBuffer, LinearBlendingFactor, VertexBuffer,
};
use glm::Mat4x4;
use native_dialog::FileDialog;
use pof::{
    properties_get_field, BspData, Insignia, NameLink, NormalId, ObjVec, ObjectId, Parser, PolyVertex, Polygon, ShieldData, SubObject, TextureId,
    Vec3d, VertexId,
};
use simplelog::*;
use std::{
    collections::HashMap,
    f32::consts::PI,
    fs::File,
    hash::Hash,
    io::{Cursor, Read},
    ops::{Deref, DerefMut},
    path::PathBuf,
    sync::mpsc::TryRecvError,
};
use ui::{PofToolsGui, TreeValue};

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

struct GlArrowhead {
    color: [f32; 4],
    transform: Mat4x4,
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

        for (_, poly) in bsp_data.collision_tree.leaves() {
            textures[poly.texture.0 as usize].push(bsp_data, poly);
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

pub struct Model {
    pof_model: pof::Model,
    /// Annoying, but 'merge' textures is best handled as simply filling this map and deferring the actual task
    /// of merging textures until write
    texture_map: HashMap<TextureId, TextureId>,
}
impl Deref for Model {
    type Target = pof::Model;

    fn deref(&self) -> &pof::Model {
        &self.pof_model
    }
}
impl DerefMut for Model {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.pof_model
    }
}
impl Model {
    pub fn clean_up(&mut self) {
        // apply changes form the texture map
        for subobj in self.pof_model.sub_objects.iter_mut() {
            for (_, poly) in subobj.bsp_data.collision_tree.leaves_mut() {
                poly.texture = self.texture_map[&poly.texture];
            }
        }

        self.pof_model.clean_up();
    }
}

impl PofToolsGui {
    fn save_model(model: &Model) -> Option<String> {
        let mut out = None;
        // use a scoped thread here, its ok to block the main window for now i guess
        crossbeam::thread::scope(|s| {
            s.spawn(|_| {
                let path = FileDialog::new()
                    .set_filename(&model.path_to_file.file_name().unwrap_or_default().to_string_lossy())
                    .add_filter("All Supported Files", &["pof", "dae", "gltf", "glb"])
                    .add_filter("Parallax Object File", &["pof"])
                    .add_filter("Digital Asset Exchange file", &["dae"])
                    .add_filter("GL Transmission Format (Embedded)", &["gltf"])
                    .add_filter("GL Transmission Format (Binary)", &["glb"])
                    .show_save_single_file();
                if let Ok(Some(path)) = path {
                    let mut file = File::create(path.clone()).unwrap();
                    match path.extension().map(|ext| ext.to_ascii_lowercase()) {
                        Some(s) if s == "glb" => model.write_gltf(&mut file, true).unwrap(),
                        Some(s) if s == "gltf" => model.write_gltf(&mut file, false).unwrap(),
                        Some(s) if s == "dae" => model.write_dae(&mut file).unwrap(),
                        Some(s) if s == "pof" => model.write(&mut file).unwrap(),
                        s => panic!("unexpected extension {:?}", s),
                    }
                    out = Some(path.file_name().and_then(|f| f.to_str()).unwrap_or("").to_string());
                }
            });
        })
        .unwrap();
        out
    }

    /// Opens a dialog to load a model. Must be run off the main thread.
    fn load_model(filepath: Option<PathBuf>) -> Result<Option<Box<Model>>, String> {
        let model = std::panic::catch_unwind(move || {
            let path = filepath.or_else(|| {
                FileDialog::new()
                    .add_filter("All supported files", &["pof", "dae", "gltf", "glb"])
                    .add_filter("COLLADA", &["dae"])
                    .add_filter("Parallax Object File", &["pof"])
                    .add_filter("GL Transmission Format", &["gltf", "glb"])
                    .show_open_single_file()
                    .unwrap()
            });

            path.map(|path| {
                let ext = path.extension().map(|ext| ext.to_ascii_lowercase());
                let filename = path.file_name().and_then(|f| f.to_str()).unwrap_or("").to_string();
                info!("Attempting to load {}", filename);
                Box::new(Model {
                    pof_model: match ext.as_ref().and_then(|ext| ext.to_str()) {
                        Some("dae") => pof::parse_dae(path),
                        Some("gltf" | "glb") => pof::parse_gltf(path),
                        Some("pof") => {
                            let file = File::open(&path).expect("TODO invalid file or smth i dunno");
                            let mut parser = Parser::new(file).expect("TODO invalid version of file or smth i dunno");
                            parser.parse(path).expect("TODO invalid pof file or smth i dunno")
                        }
                        _ => todo!(),
                    },
                    texture_map: HashMap::new(),
                })
            })
        });
        model.map_err(|panic| *panic.downcast().unwrap())
    }

    /// opens a thread which opens the dialog and starts parsing a model
    fn start_loading_model(&mut self, filepath: Option<PathBuf>) {
        let (sender, receiver) = std::sync::mpsc::channel();
        self.model_loading_thread = Some(receiver);

        // the model loading thread
        std::thread::spawn(move || drop(sender.send(Self::load_model(filepath))));
    }

    fn handle_model_loading_thread(&mut self, display: &Display) -> bool {
        if let Some(thread) = &self.model_loading_thread {
            let response = thread.try_recv();
            match response {
                Ok(Ok(Some(data))) => {
                    self.model = data;
                    self.finish_loading_model(display);

                    self.model_loading_thread = None;
                    return true;
                }
                Err(TryRecvError::Disconnected) => self.model_loading_thread = None,
                Ok(Ok(None)) => self.model_loading_thread = None,
                Ok(Err(_)) => self.model_loading_thread = None,

                Err(TryRecvError::Empty) => {}
            }
        }

        false
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

        self.model.recheck_warnings(pof::Set::All);
        self.model.recheck_errors(pof::Set::All);
        for i in 0..self.model.textures.len() {
            self.model.texture_map.insert(TextureId(i as u32), TextureId(i as u32));
        }
        self.ui_state.tree_view_selection = Default::default();
        self.ui_state.refresh_properties_panel(&self.model);
        self.camera_heading = 2.7;
        self.camera_pitch = -0.4;
        self.camera_offset = Vec3d::ZERO;
        self.camera_scale = self.model.header.max_radius * 1.5;
        self.ui_state.last_selected_subobj = self.model.header.detail_levels.first().copied();
        self.ui_state.tree_view_selection = TreeValue::Header;

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

    pub fn rebuild_all_subobj_buffers(&mut self, display: &Display) {
        let ids = (0..self.model.sub_objects.len()).map(|idx| ObjectId(idx as u32)).collect();
        self.rebuild_subobj_buffers(display, ids);
    }

    pub fn rebuild_all_insignia_buffers(&mut self, display: &Display) {
        for (i, buffer) in self.buffer_insignias.iter_mut().enumerate() {
            *buffer = GlBufferedInsignia::new(display, &self.model.insignias[i]);
        }
    }

    pub fn rebuild_shield_buffer(&mut self, display: &Display) {
        if let Some(shield) = &self.model.shield_data {
            self.buffer_shield = Some(GlBufferedShield::new(display, shield));
        } else {
            self.buffer_shield = None;
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

    let event_loop = glutin::event_loop::EventLoopBuilder::with_user_event().build();
    let display = create_display(&event_loop);
    let mut pt_gui = PofToolsGui::new(&display);

    // this creates the raw bytes from png, how i make the icon
    // File::create("zforward.raw")
    //     .unwrap()
    //     .write_all(&image::open("zforward.png").unwrap().into_rgba8().to_vec());

    let mut args = std::env::args();
    args.next();
    let path = args.next().map(|arg| PathBuf::from(arg.as_str()));

    let mut egui = egui_glium::EguiGlium::new(&display, &event_loop);

    pt_gui.start_loading_model(path);

    let model = &pt_gui.model;

    pt_gui.camera_heading = 2.7;
    pt_gui.camera_pitch = -0.4;
    pt_gui.camera_offset = Vec3d::ZERO;
    pt_gui.camera_scale = model.header.max_radius * 2.0;

    let mut undo_history = undo::History::new();

    let mut errored = None;
    info!("Beginning event loop...");

    event_loop.run(move |event, _, control_flow| {
        let mut catch_redraw = || {
            let redraw = || {
                // handle whether the thread which handles loading has responded (if it exists)
                if pt_gui.handle_model_loading_thread(&display) {
                    undo_history.clear();
                }

                pt_gui.handle_texture_loading_thread(&display);

                let repaint_after = egui.run(&display, |ctx| pt_gui.show_ui(ctx, &display, &mut undo_history));

                *control_flow = match repaint_after {
                    time if time.is_zero() => {
                        display.gl_window().window().request_redraw();
                        glutin::event_loop::ControlFlow::Poll
                    }
                    time => match std::time::Instant::now().checked_add(time) {
                        Some(next_frame_time) => glutin::event_loop::ControlFlow::WaitUntil(next_frame_time),
                        None => glutin::event_loop::ControlFlow::Wait,
                    },
                };

                {
                    use glium::Surface as _;
                    let mut target = display.draw();

                    target.clear_color_and_depth((0.0, 0.0, 0.0, 1.0), 1.0);

                    // undo/redo
                    let input = egui.egui_ctx.input();
                    let ctrl = input.modifiers.ctrl;
                    let z = input.key_pressed(egui::Key::Z);
                    drop(input);
                    if egui.egui_ctx.memory().focus().is_none() && ctrl && z {
                        undo_history.undo(&mut *pt_gui.model);
                        pt_gui.sanitize_ui_state();
                    }

                    let model = &pt_gui.model;

                    // set up the camera matrix
                    let perspective_matrix = {
                        let (width, height) = target.get_dimensions();
                        let aspect_ratio = height as f32 / width as f32;

                        if pt_gui.camera_orthographic {
                            let zfar = (model.header.max_radius) * 2.0;
                            let znear = (model.header.max_radius) * -2.0;
                            let f = 1.5 / pt_gui.camera_scale;
                            Mat4x4::from([
                                [f * aspect_ratio, 0.0, 0.0, 0.0],
                                [0.0, f, 0.0, 0.0],
                                [0.0, 0.0, (2.0) / (zfar - znear), 0.0],
                                [0.0, 0.0, -(zfar + znear) / (zfar - znear), 1.0],
                            ])
                        } else {
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
                        }
                    };

                    let mut view_mat = glm::rotation(pt_gui.camera_pitch, &glm::vec3(1., 0., 0.)); // pitch
                    view_mat *= glm::rotation(pt_gui.camera_heading, &glm::vec3(0., 1., 0.)); // heading

                    let rot_only_view_mat = view_mat;

                    // handle user interactions like rotating the camera
                    let rect = egui.egui_ctx.available_rect(); // the rectangle not covered by egui UI, i.e. the 3d viewport
                    let input = egui.egui_ctx.input();
                    let mouse_pos = input.pointer.hover_pos();
                    let mouse_in_3d_viewport = mouse_pos.map_or(false, |hover_pos| rect.contains(hover_pos));
                    if rect.is_positive() {
                        let last_click_pos = input.pointer.press_origin();
                        let clicked_in_3d_viewport = last_click_pos.map_or(false, |hover_pos| rect.contains(hover_pos));
                        if clicked_in_3d_viewport {
                            if !input.modifiers.shift && input.pointer.button_down(egui::PointerButton::Secondary) {
                                pt_gui.camera_heading += input.pointer.delta().x * -0.01;
                                pt_gui.camera_pitch += input.pointer.delta().y * -0.01;
                            } else if input.pointer.button_down(egui::PointerButton::Middle)
                                || input.modifiers.shift && input.pointer.button_down(egui::PointerButton::Secondary)
                            {
                                let x = input.pointer.delta().x * -0.003 * pt_gui.camera_scale; // for some reason x gets inverted
                                let y = input.pointer.delta().y * 0.003 * pt_gui.camera_scale;

                                pt_gui.camera_offset += view_mat.transpose().transform_vector(&glm::vec3(x, y, 0.)).into();
                            }
                        }
                        if mouse_in_3d_viewport {
                            pt_gui.camera_scale *= 1.0 + (input.scroll_delta.y * -0.001)
                        }
                    }
                    drop(input);

                    if !pt_gui.camera_orthographic {
                        view_mat.append_translation_mut(&glm::vec3(0.0, 0.0, pt_gui.camera_scale));
                    }

                    view_mat.prepend_translation_mut(&glm::vec3(-pt_gui.camera_offset.x, -pt_gui.camera_offset.y, -pt_gui.camera_offset.z));
                    view_mat.prepend_translation_mut(&glm::vec3(-model.visual_center.x, -model.visual_center.y, -model.visual_center.z));

                    let mouse_pos =
                        egui.egui_ctx.input().pointer.hover_pos().map(|pos| {
                            ((pos.x / target.get_dimensions().0 as f32) * 2.0 - 1.0, (pos.y / target.get_dimensions().1 as f32) * 2.0 - 1.0)
                        });
                    let mouse_vec = {
                        mouse_pos.map(|pos| {
                            let matrix = (perspective_matrix * view_mat).try_inverse().unwrap();
                            let pos1 = &matrix * Vec3d::new(pos.0, -pos.1, 0.0);
                            let pos2 = &matrix * Vec3d::new(pos.0, -pos.1, 1.0);
                            (pos1, pos2)
                        })
                    };

                    if mouse_in_3d_viewport {
                        pt_gui.hover_lollipop = pt_gui.get_hover_lollipop(mouse_vec);
                    } else {
                        pt_gui.hover_lollipop = None;
                    }

                    // start the drag/selection if the user clicked on a lollipop
                    if let Some((vec1, vec2)) = mouse_vec {
                        if egui.egui_ctx.input().pointer.button_clicked(PointerButton::Primary) {
                            if let Some(lollipop) = pt_gui.hover_lollipop {
                                pt_gui.drag_lollipop = Some(lollipop);
                                let vec = (vec1 - vec2).normalize();
                                pt_gui.drag_axis = {
                                    let modifiers = egui.egui_ctx.input().modifiers;
                                    match (modifiers.shift, modifiers.ctrl, modifiers.alt) {
                                        (true, _, _) => DragAxis::YZ,
                                        (_, true, _) => DragAxis::XZ,
                                        (_, _, true) => DragAxis::XY,
                                        _ => match (vec.x, vec.y, vec.z) {
                                            _ if vec.x.abs() > vec.y.abs() && vec.x.abs() > vec.z.abs() => DragAxis::YZ,
                                            _ if vec.y.abs() > vec.x.abs() && vec.y.abs() > vec.z.abs() => DragAxis::XZ,
                                            _ if vec.z.abs() > vec.x.abs() && vec.z.abs() > vec.y.abs() => DragAxis::XY,
                                            _ => DragAxis::YZ,
                                        },
                                    }
                                };
                                pt_gui.drag_start = *lollipop.get_position_ref(&mut pt_gui.model).unwrap();
                                if let TreeValue::Turrets(TurretTreeValue::TurretPoint(i, _)) = lollipop {
                                    pt_gui.drag_start += pt_gui.model.get_total_subobj_offset(pt_gui.model.turrets[i].gun_obj);
                                }

                                pt_gui.select_new_tree_val(lollipop);
                                pt_gui.ui_state.properties_panel_dirty = true;
                            }
                        }
                    }

                    // continue the drag if the user is still dragging the lollipop
                    if let Some(drag_lollipop) = pt_gui.drag_lollipop {
                        // only drag if they've sufficiently moved the mouse
                        if !pt_gui.actually_dragging
                            && egui.egui_ctx.input().pointer.press_origin().map_or(false, |origin| {
                                egui.egui_ctx
                                    .input()
                                    .pointer
                                    .hover_pos()
                                    .map_or(false, |current_pos| current_pos.distance(origin) > 4.)
                            })
                        {
                            pt_gui.actually_dragging = true;
                        }

                        if pt_gui.actually_dragging {
                            // take into account turret offset
                            let mut maybe_turret_offset = Vec3d::ZERO;
                            if let Some(TreeValue::Turrets(TurretTreeValue::TurretPoint(i, _))) = pt_gui.drag_lollipop {
                                maybe_turret_offset = pt_gui.model.get_total_subobj_offset(pt_gui.model.turrets[i].gun_obj);
                            }

                            if let Some(vec) = drag_lollipop.get_position_ref(&mut pt_gui.model) {
                                // if the user pressed a hotkey, reset drag_start and use a new axis
                                let modifiers = egui.egui_ctx.input().modifiers;
                                pt_gui.drag_axis = match (modifiers.shift, modifiers.ctrl, modifiers.alt) {
                                    (true, _, _) if pt_gui.drag_axis != DragAxis::YZ => {
                                        pt_gui.drag_start = *vec + maybe_turret_offset;
                                        DragAxis::YZ
                                    }
                                    (_, true, _) if pt_gui.drag_axis != DragAxis::XZ => {
                                        pt_gui.drag_start = *vec + maybe_turret_offset;
                                        DragAxis::XZ
                                    }
                                    (_, _, true) if pt_gui.drag_axis != DragAxis::XY => {
                                        pt_gui.drag_start = *vec + maybe_turret_offset;
                                        DragAxis::XY
                                    }
                                    _ => pt_gui.drag_axis,
                                };

                                let new_pos: Option<Vec3d> = {
                                    let mouse_vec = mouse_vec.unwrap();
                                    let t = match pt_gui.drag_axis {
                                        DragAxis::YZ => ((*vec + maybe_turret_offset).x - mouse_vec.0.x) / (mouse_vec.1.x - mouse_vec.0.x),
                                        DragAxis::XZ => ((*vec + maybe_turret_offset).y - mouse_vec.0.y) / (mouse_vec.1.y - mouse_vec.0.y),
                                        DragAxis::XY => ((*vec + maybe_turret_offset).z - mouse_vec.0.z) / (mouse_vec.1.z - mouse_vec.0.z),
                                    };
                                    let hover_pos = egui.egui_ctx.input().pointer.hover_pos();
                                    let in_3d_viewport = hover_pos.map_or(false, |hover_pos| egui.egui_ctx.available_rect().contains(hover_pos));
                                    if mouse_pos.is_none()
                                        || !(-1.0..=1.0).contains(&t)
                                        || !egui.egui_ctx.input().pointer.primary_down()
                                        || !in_3d_viewport
                                    {
                                        pt_gui.drag_lollipop = None;
                                        pt_gui.actually_dragging = false;
                                        None
                                    } else {
                                        Some(mouse_vec.0 + (mouse_vec.1 - mouse_vec.0) * t)
                                    }
                                };

                                if let Some(new_pos) = new_pos {
                                    let delta_vec = new_pos - maybe_turret_offset - *vec;
                                    undo_history
                                        .apply(&mut *pt_gui.model, UndoAction::MoveLollipop { tree_val: drag_lollipop, delta_vec })
                                        .unwrap();

                                    pt_gui.ui_state.refresh_properties_panel(&pt_gui.model);
                                    pt_gui.ui_state.viewport_3d_dirty = true;
                                }
                            }
                        }

                        // for when the user releases without dragging
                        if !egui.egui_ctx.input().pointer.primary_down() {
                            pt_gui.drag_lollipop = None;
                            pt_gui.actually_dragging = false;
                        }
                    }

                    //
                    // TIME TO RENDER STUFF =======================================================================================
                    //

                    // start with the 'orient billboards', the text in the distance that says forward, left, etc
                    for i in 0..6 {
                        let mut matrix = rot_only_view_mat;
                        match i {
                            1 => matrix *= glm::rotation(std::f32::consts::PI, &glm::vec3(0.0, 1.0, 0.0)),
                            2 => matrix *= glm::rotation(-std::f32::consts::FRAC_PI_2, &glm::vec3(0.0, 1.0, 0.0)),
                            3 => matrix *= glm::rotation(std::f32::consts::FRAC_PI_2, &glm::vec3(0.0, 1.0, 0.0)),
                            4 => matrix *= glm::rotation(-std::f32::consts::FRAC_PI_2, &glm::vec3(1.0, 0.0, 0.0)),
                            5 => matrix *= glm::rotation(std::f32::consts::FRAC_PI_2, &glm::vec3(1.0, 0.0, 0.0)),
                            _ => (),
                        }

                        if !pt_gui.camera_orthographic {
                            matrix.append_translation_mut(&glm::vec3(0.0, 0.0, 0.001f32));
                            matrix.prepend_translation_mut(&glm::vec3(0.0, 0.0, 10.0f32));
                        } else {
                            matrix.append_scaling_mut(pt_gui.model.header.max_radius * 0.3);
                            matrix.prepend_translation_mut(&glm::vec3(0.0, 0.0, 6.5));
                        }

                        let vert_matrix: [[f32; 4]; 4] = (perspective_matrix * matrix).into();
                        let uniforms = glium::uniform! {
                            vert_matrix: vert_matrix,
                            tex: &pt_gui.graphics.orient_billboards[i],
                        };

                        target
                            .draw(
                                &pt_gui.graphics.square_verts,
                                &pt_gui.graphics.square_indices,
                                &pt_gui.graphics.flat_textured_material_shader,
                                &uniforms,
                                &pt_gui.graphics.orient_billboards_params,
                            )
                            .unwrap();
                    }

                    // maybe redo lollipops and stuff
                    pt_gui.maybe_recalculate_3d_helpers(&display);

                    let model = &pt_gui.model;

                    // brighten up the dark bits so wireframe is easier to see
                    let mut dark_color;
                    if pt_gui.display_mode == DisplayMode::Wireframe {
                        pt_gui.graphics.default_material_draw_params.polygon_mode = glium::draw_parameters::PolygonMode::Line;
                        pt_gui.graphics.default_material_draw_params.backface_culling = glium::draw_parameters::BackfaceCullingMode::CullingDisabled;
                        dark_color = [0.2, 0.2, 0.2f32];
                    } else {
                        pt_gui.graphics.default_material_draw_params.polygon_mode = glium::draw_parameters::PolygonMode::Fill;
                        pt_gui.graphics.default_material_draw_params.backface_culling =
                            glium::draw_parameters::BackfaceCullingMode::CullCounterClockwise;
                        dark_color = [0.01, 0.01, 0.01f32];
                    }

                    // dim down the bright bits when lollipops are on screen
                    let light_color;
                    match &pt_gui.ui_state.tree_view_selection {
                        TreeValue::Thrusters(_)
                        | TreeValue::Weapons(_)
                        | TreeValue::DockingBays(_)
                        | TreeValue::Glows(_)
                        | TreeValue::SpecialPoints(_)
                        | TreeValue::Turrets(_)
                        | TreeValue::Paths(_)
                        | TreeValue::EyePoints(_)
                        | TreeValue::VisualCenter => {
                            light_color = [0.3, 0.3, 0.3f32];
                            if pt_gui.display_mode == DisplayMode::Wireframe {
                                dark_color = [0.05, 0.05, 0.05f32];
                            }
                        }
                        _ => light_color = [1.0, 1.0, 1.0f32],
                    }

                    let light_vec = glm::vec3(0.5, 1.0, -1.0);

                    let displayed_subobjects =
                        get_list_of_display_subobjects(model, pt_gui.ui_state.tree_view_selection, pt_gui.ui_state.last_selected_subobj);

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
                                            &pt_gui.graphics.textured_material_shader,
                                            &uniforms,
                                            &pt_gui.graphics.default_material_draw_params,
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
                                            &pt_gui.graphics.default_material_shader,
                                            &uniforms,
                                            &pt_gui.graphics.default_material_draw_params,
                                        )
                                        .unwrap();
                                }
                            }
                        }
                    }

                    // maybe draw the insignias
                    if let TreeValue::Insignia(insignia_select) = pt_gui.tree_view_selection {
                        let (current_detail_level, current_insignia_idx) = match insignia_select {
                            InsigniaTreeValue::Header => (0, None),
                            InsigniaTreeValue::Insignia(idx) => (pt_gui.model.insignias[idx].detail_level, Some(idx)),
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
                                        &pt_gui.graphics.default_material_shader,
                                        &uniforms,
                                        &pt_gui.graphics.default_material_draw_params,
                                    )
                                    .unwrap();
                            }
                        }
                    }

                    // maybe draw the shield
                    if let TreeValue::Shield = pt_gui.tree_view_selection {
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
                                .draw(
                                    (&shield.vertices, &shield.normals),
                                    &shield.indices,
                                    &pt_gui.graphics.shield_shader,
                                    &uniforms,
                                    &pt_gui.graphics.shield_draw_params,
                                )
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
                    if let TreeValue::SubObjects(SubObjectTreeValue::SubObject(id)) = pt_gui.tree_view_selection {
                        obj_id = Some(id);

                        //      DEBUG - Draw BSP node bounding boxes
                        //      This is quite useful but incredibly ineffcient
                        //      TODO make this more efficient
                        //
                        // let mut node_stack = vec![(&pt_gui.model.sub_objects[id].bsp_data.collision_tree, 0u32)];
                        // while let Some((node, depth)) = node_stack.pop() {
                        //     let bbox = match node {
                        //         BspNode::Split { bbox, front, back, .. } => {
                        //             node_stack.push((front, depth + 1));
                        //             node_stack.push((back, depth + 1));
                        //             bbox
                        //         }
                        //         BspNode::Leaf { bbox, .. } => bbox,
                        //         BspNode::Empty => continue,
                        //     };

                        //     let mut mat = glm::scaling(&(bbox.max - bbox.min).into());
                        //     mat.append_translation_mut(&(bbox.min + pt_gui.model.get_total_subobj_offset(id)).into());
                        //     let color = 2.0 / (1.5f32.powf(depth as f32));
                        //     let matrix = view_mat * mat;
                        //     let vert_matrix: [[f32; 4]; 4] = (perspective_matrix * matrix).into();

                        //     let uniforms = glium::uniform! {
                        //         vert_matrix: vert_matrix,
                        //         lollipop_color: [color, color, color, 1.0f32],
                        //     };

                        //     target
                        //         .draw(&box_verts, &box_indices, &lollipop_stick_shader, &uniforms, &lollipop_stick_params)
                        //         .unwrap();
                        // }
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
                            .draw(
                                &pt_gui.graphics.box_verts,
                                &pt_gui.graphics.box_indices,
                                &pt_gui.graphics.wireframe_shader,
                                &uniforms,
                                &pt_gui.graphics.wireframe_params,
                            )
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
                                .draw(
                                    &pt_gui.graphics.circle_verts,
                                    &pt_gui.graphics.circle_indices,
                                    &pt_gui.graphics.wireframe_shader,
                                    &uniforms,
                                    &pt_gui.graphics.wireframe_params,
                                )
                                .unwrap();
                        }
                    }

                    // draw the 'drag axes' if the user is dragging a lollipop
                    if pt_gui.drag_lollipop.is_some() && pt_gui.actually_dragging {
                        let mut mat = view_mat;
                        mat.prepend_translation_mut(&pt_gui.drag_start.into());
                        mat.prepend_scaling_mut(pt_gui.model.header.max_radius * 2.0);

                        let vert_matrix: [[f32; 4]; 4] = (perspective_matrix * mat).into();

                        let (vert1, vert2, vert3, vert4, color1, color2) = match pt_gui.drag_axis {
                            DragAxis::YZ => (
                                OCTAHEDRON_VERTS[2],
                                OCTAHEDRON_VERTS[3],
                                OCTAHEDRON_VERTS[4],
                                OCTAHEDRON_VERTS[5],
                                [0.0, 1.0, 0.0, 1.0f32],
                                [0.1, 0.1, 1.0, 1.0f32],
                            ),
                            DragAxis::XZ => (
                                OCTAHEDRON_VERTS[0],
                                OCTAHEDRON_VERTS[1],
                                OCTAHEDRON_VERTS[4],
                                OCTAHEDRON_VERTS[5],
                                [1.0, 0.0, 0.0, 1.0f32],
                                [0.1, 0.1, 1.0, 1.0f32],
                            ),
                            DragAxis::XY => (
                                OCTAHEDRON_VERTS[0],
                                OCTAHEDRON_VERTS[1],
                                OCTAHEDRON_VERTS[2],
                                OCTAHEDRON_VERTS[3],
                                [0.0, 1.0, 0.0, 1.0f32],
                                [1.0, 0.0, 0.0, 1.0f32],
                            ),
                        };

                        let uniforms = glium::uniform! {
                            vert_matrix: vert_matrix,
                            lollipop_color: color1,
                        };
                        target
                            .draw(
                                &glium::VertexBuffer::new(&display, &[vert1, vert2]).unwrap(),
                                glium::index::NoIndices(glium::index::PrimitiveType::LinesList),
                                &pt_gui.graphics.lollipop_stick_shader,
                                &uniforms,
                                &pt_gui.graphics.drag_axis_params,
                            )
                            .unwrap();

                        let uniforms = glium::uniform! {
                            vert_matrix: vert_matrix,
                            lollipop_color: color2,
                        };
                        target
                            .draw(
                                &glium::VertexBuffer::new(&display, &[vert3, vert4]).unwrap(),
                                glium::index::NoIndices(glium::index::PrimitiveType::LinesList),
                                &pt_gui.graphics.lollipop_stick_shader,
                                &uniforms,
                                &pt_gui.graphics.drag_axis_params,
                            )
                            .unwrap();
                    }

                    // don't display lollipops if you're in header or subobjects, unless display_origin is on, since that's the only lollipop they have

                    let display_lollipops = (!matches!(pt_gui.ui_state.tree_view_selection, TreeValue::Header)
                        && !matches!(pt_gui.ui_state.tree_view_selection, TreeValue::SubObjects(_)))
                        || pt_gui.display_origin
                        || pt_gui.display_uvec_fvec;

                    if display_lollipops {
                        for lollipop_group in &pt_gui.lollipops {
                            if let TreeValue::Paths(_) = pt_gui.ui_state.tree_view_selection {
                                pt_gui.graphics.lollipop_params.blend = glium::Blend::alpha_blending();
                            } else {
                                pt_gui.graphics.lollipop_params.blend = ADDITIVE_BLEND;
                            }
                            let vert_matrix: [[f32; 4]; 4] = (perspective_matrix * view_mat).into();

                            let uniforms = glium::uniform! {
                                vert_matrix: vert_matrix,
                                lollipop_color: lollipop_group.color,
                            };

                            target
                                .draw(
                                    (&pt_gui.graphics.icosphere_verts, lollipop_group.lolly_vertices.per_instance().unwrap()),
                                    &pt_gui.graphics.icosphere_indices,
                                    &pt_gui.graphics.lollipop_shader,
                                    &uniforms,
                                    &pt_gui.graphics.lollipop_params,
                                )
                                .unwrap();
                            target
                                .draw(
                                    &lollipop_group.stick_vertices,
                                    lollipop_group.stick_indices,
                                    &pt_gui.graphics.lollipop_stick_shader,
                                    &uniforms,
                                    &pt_gui.graphics.lollipop_stick_params,
                                )
                                .unwrap();

                            if !matches!(pt_gui.ui_state.tree_view_selection, TreeValue::Paths(_)) {
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
                                        (&pt_gui.graphics.icosphere_verts, lollipop_group.lolly_vertices.per_instance().unwrap()),
                                        &pt_gui.graphics.icosphere_indices,
                                        &pt_gui.graphics.lollipop_shader,
                                        &uniforms,
                                        &pt_gui.graphics.lollipop_rev_depth_params,
                                    )
                                    .unwrap();
                            }
                        }
                        for arrowhead in &pt_gui.arrowheads {
                            let vert_matrix: [[f32; 4]; 4] = (perspective_matrix * view_mat * arrowhead.transform).into();
                            let uniforms = glium::uniform! {
                                vert_matrix: vert_matrix,
                                lollipop_color: arrowhead.color,
                            };
                            target
                                .draw(
                                    &pt_gui.graphics.arrowhead_verts,
                                    &pt_gui.graphics.arrowhead_indices,
                                    &pt_gui.graphics.arrowhead_shader,
                                    &uniforms,
                                    &pt_gui.graphics.arrowhead_draw_params,
                                )
                                .unwrap();
                        }
                    }

                    // draw the turret fov angular frustum thing
                    if let TreeValue::SubObjects(SubObjectTreeValue::SubObject(id)) = pt_gui.tree_view_selection {
                        if let Some(val) = properties_get_field(&model.sub_objects[id].properties, "$fov").and_then(|str| str.parse::<f32>().ok()) {
                            let max_fov = properties_get_field(&model.sub_objects[id].properties, "$max_fov")
                                .and_then(|str| str.parse::<f32>().ok())
                                .unwrap_or(90.0);
                            let base_fov = properties_get_field(&model.sub_objects[id].properties, "$base_fov")
                                .and_then(|str| str.parse::<f32>().ok())
                                .unwrap_or(360.0);

                            if let Some((turret_idx, _)) = pt_gui.model.turrets.iter().enumerate().find(|(_, turret)| turret.base_obj == id) {
                                let mut turret_mat = pt_gui.model.turret_matrix(turret_idx);
                                let offset = pt_gui.model.get_total_subobj_offset(id);
                                turret_mat.append_translation_mut(&offset.into());
                                let vert_matrix: [[f32; 4]; 4] = (perspective_matrix * view_mat * turret_mat).into();
                                let uniforms = glium::uniform! {
                                    vert_matrix: vert_matrix,
                                    world_offset: [offset.x, offset.y, offset.z],
                                    scale: pt_gui.model.header.max_radius * 0.4,
                                    fov: val * 0.5 * PI / 180.0,
                                    max_fov: (90.0 - max_fov) * PI / 180.0,
                                    base_fov: base_fov * 0.5 * PI / 180.0,
                                    lollipop_color: [1.0, 0.0, 0.0, 1.0f32],
                                };
                                target
                                    .draw(
                                        &pt_gui.graphics.frustum_verts,
                                        glium::index::NoIndices(glium::index::PrimitiveType::LinesList),
                                        &pt_gui.graphics.fov_shader,
                                        &uniforms,
                                        &pt_gui.graphics.lollipop_stick_params,
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
                let repaint_after = egui.run(&display, |ctx| {
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

                // Needs testing
                *control_flow = match repaint_after {
                    time if time.is_zero() => {
                        display.gl_window().window().request_redraw();
                        glutin::event_loop::ControlFlow::Poll
                    }
                    time => match std::time::Instant::now().checked_add(time) {
                        Some(next_frame_time) => glutin::event_loop::ControlFlow::WaitUntil(next_frame_time),
                        None => glutin::event_loop::ControlFlow::Wait,
                    },
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
fn get_list_of_display_subobjects(model: &Model, tree_selection: TreeValue, last_selected_subobj: Option<ObjectId>) -> ObjVec<bool> {
    let mut out = ObjVec(vec![false; model.sub_objects.len()]);

    if model.sub_objects.is_empty() {
        return out;
    }

    if let TreeValue::Insignia(InsigniaTreeValue::Insignia(idx)) = tree_selection {
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

        let displaying_destroyed_models = model.sub_objects[last_selected_subobj].is_destroyed_model();

        model.do_for_recursive_subobj_children(top_level_parent, &mut |subobj| {
            if let Some(id) = subobj.parent() {
                let has_a_destroyed_version = subobj.name_links.iter().any(|link| matches!(link, NameLink::DestroyedVersion(_)));
                let parent = &model.sub_objects[id];
                let parent_has_a_destroyed_version = parent.name_links.iter().any(|link| matches!(link, NameLink::DestroyedVersion(_)));

                if (!parent_has_a_destroyed_version || (displaying_destroyed_models == parent.is_destroyed_model()))
                    && (!has_a_destroyed_version || (displaying_destroyed_models == subobj.is_destroyed_model()))
                {
                    out[subobj.obj_id] = true;
                }
            } else {
                out[subobj.obj_id] = true;
            }
        });

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
    fn get_hover_lollipop(&mut self, mouse_vec: Option<(Vec3d, Vec3d)>) -> Option<TreeValue> {
        let (camera_vec, mouse_vec) = mouse_vec?;

        let mut best_approach = 0.05 * self.model.header.max_radius;
        let mut result = None;
        let mut proximity_test = |test_point: Vec3d, value: TreeValue| {
            let closest_approach = closest_approach(camera_vec, mouse_vec, test_point);
            let proximity_modified = (closest_approach - test_point).magnitude();
            if proximity_modified < best_approach {
                best_approach = proximity_modified;
                result = Some(value);
            }
        };
        match self.ui_state.tree_view_selection {
            TreeValue::Weapons(WeaponTreeValue::Header) => {
                for (i, bank) in self.model.primary_weps.iter().enumerate() {
                    for (j, point) in bank.iter().enumerate() {
                        proximity_test(point.position, TreeValue::Weapons(WeaponTreeValue::PriBankPoint(i, j)));
                    }
                }
                for (i, bank) in self.model.secondary_weps.iter().enumerate() {
                    for (j, point) in bank.iter().enumerate() {
                        proximity_test(point.position, TreeValue::Weapons(WeaponTreeValue::SecBankPoint(i, j)));
                    }
                }
            }
            TreeValue::Weapons(WeaponTreeValue::SecBank(_))
            | TreeValue::Weapons(WeaponTreeValue::SecBankPoint(..))
            | TreeValue::Weapons(WeaponTreeValue::SecHeader) => {
                for (i, bank) in self.model.secondary_weps.iter().enumerate() {
                    for (j, point) in bank.iter().enumerate() {
                        proximity_test(point.position, TreeValue::Weapons(WeaponTreeValue::SecBankPoint(i, j)));
                    }
                }
            }
            TreeValue::Weapons(WeaponTreeValue::PriBank(_))
            | TreeValue::Weapons(WeaponTreeValue::PriBankPoint(..))
            | TreeValue::Weapons(WeaponTreeValue::PriHeader) => {
                for (i, bank) in self.model.primary_weps.iter().enumerate() {
                    for (j, point) in bank.iter().enumerate() {
                        proximity_test(point.position, TreeValue::Weapons(WeaponTreeValue::PriBankPoint(i, j)));
                    }
                }
            }
            TreeValue::DockingBays(_) => {
                for (i, dock) in self.model.docking_bays.iter().enumerate() {
                    proximity_test(dock.position, TreeValue::DockingBays(DockingTreeValue::Bay(i)));
                }
            }
            TreeValue::Thrusters(_) => {
                for (i, bank) in self.model.thruster_banks.iter().enumerate() {
                    for (j, point) in bank.glows.iter().enumerate() {
                        proximity_test(point.position, TreeValue::Thrusters(ThrusterTreeValue::BankPoint(i, j)));
                    }
                }
            }
            TreeValue::Glows(_) => {
                for (i, bank) in self.model.glow_banks.iter().enumerate() {
                    for (j, point) in bank.glow_points.iter().enumerate() {
                        proximity_test(point.position, TreeValue::Glows(GlowTreeValue::BankPoint(i, j)));
                    }
                }
            }
            TreeValue::SpecialPoints(_) => {
                for (i, point) in self.model.special_points.iter().enumerate() {
                    proximity_test(point.position, TreeValue::SpecialPoints(SpecialPointTreeValue::Point(i)));
                }
            }
            TreeValue::Turrets(_) => {
                for (i, turret) in self.model.turrets.iter().enumerate() {
                    for (j, point) in turret.fire_points.iter().enumerate() {
                        let point = *point + self.model.get_total_subobj_offset(turret.gun_obj);
                        proximity_test(point, TreeValue::Turrets(TurretTreeValue::TurretPoint(i, j)));
                    }
                }
            }
            TreeValue::Paths(_) => {
                for (i, path) in self.model.paths.iter().enumerate() {
                    for (j, point) in path.points.iter().enumerate() {
                        proximity_test(point.position, TreeValue::Paths(PathTreeValue::PathPoint(i, j)));
                    }
                }
            }
            TreeValue::EyePoints(_) => {
                for (i, point) in self.model.eye_points.iter().enumerate() {
                    proximity_test(point.position, TreeValue::EyePoints(EyeTreeValue::EyePoint(i)));
                }
            }
            TreeValue::VisualCenter => proximity_test(self.model.visual_center, TreeValue::VisualCenter),
            _ => (),
        }
        result
    }

    fn maybe_recalculate_3d_helpers(&mut self, display: &Display) {
        if self.buffer_objects.is_empty() {
            return;
        }

        // for lollipop hovering we have to do this every frame, maybe address this later but performance still seems to be ok
        // if !(self.ui_state.viewport_3d_dirty || (self.glow_point_simulation && matches!(self.tree_view_selection, TreeValue::Glows(_)))) {
        //     return;
        // }

        for buffers in &mut self.buffer_objects {
            for buffer in &mut buffers.buffers {
                buffer.tint_val = 0.0;
            }
        }

        self.lollipops.clear();
        self.arrowheads.clear();

        let model = &self.model;
        let hover_lollipop = self.drag_lollipop.or(self.hover_lollipop);

        const UNSELECTED: usize = 0;
        const SELECTED_POINT: usize = 1;
        const SELECTED_BANK: usize = 2;

        // determine what bank/point is selected, if any
        // push the according lollipop positions/normals based on the points positions/normals
        // push into 3 separate vectors, for 3 separate colors depending on selection state
        match self.ui_state.tree_view_selection {
            TreeValue::SubObjects(SubObjectTreeValue::SubObject(obj_id)) => {
                for buffers in &mut self.buffer_objects {
                    if buffers.obj_id == obj_id {
                        for buffer in &mut buffers.buffers {
                            buffer.tint_val = 0.2;
                        }
                    }
                }

                let radius = model.sub_objects[obj_id].radius;
                let size = 0.05 * radius;
                let pos = model.get_total_subobj_offset(obj_id);

                let mut ball_origin = GlLollipopsBuilder::new(LOLLIPOP_SELECTED_POINT_COLOR);
                // Origin lollipop (ball only)
                ball_origin.push(pos, Vec3d::ZERO, size);
                let ball_origin = ball_origin.finish(display);
                self.lollipops = vec![ball_origin];

                if let Some((uvec, fvec)) = model.sub_objects[obj_id].uvec_fvec() {
                    // Set up arrowhead sticks
                    let stick_length = 2. * radius;
                    // Blue lollipop (stick only) for uvec
                    let mut stick_uvec = GlLollipopsBuilder::new(UVEC_COLOR);
                    stick_uvec.push(pos, uvec * stick_length, 0.);
                    let stick_uvec = stick_uvec.finish(display);
                    self.lollipops.push(stick_uvec);
                    // Green lollipop (stick only) for fvec
                    let mut stick_fvec = GlLollipopsBuilder::new(FVEC_COLOR);
                    stick_fvec.push(pos, fvec * stick_length, 0.);
                    let stick_fvec = stick_fvec.finish(display);
                    self.lollipops.push(stick_fvec);
                    // Set up arrowheads
                    let uvec_pos = pos + uvec * stick_length;
                    let fvec_pos = pos + fvec * stick_length;
                    let uvec_matrix = {
                        let mut m = glm::translation::<f32>(&uvec_pos.into());
                        m *= uvec.to_rotation_matrix();
                        m *= glm::scaling(&glm::vec3(radius * 0.5, radius * 0.5, radius * 0.5));
                        m
                    };
                    let fvec_matrix = {
                        let mut m = glm::translation::<f32>(&fvec_pos.into());
                        m *= fvec.to_rotation_matrix();
                        m *= glm::scaling(&glm::vec3(radius * 0.5, radius * 0.5, radius * 0.5));
                        m
                    };
                    self.arrowheads.push(GlArrowhead { color: UVEC_COLOR, transform: uvec_matrix });
                    self.arrowheads.push(GlArrowhead { color: FVEC_COLOR, transform: fvec_matrix });
                }
            }
            TreeValue::Textures(TextureTreeValue::Texture(tex)) => {
                for buffers in &mut self.buffer_objects {
                    for buffer in &mut buffers.buffers {
                        if buffer.texture_id.map(|id| self.model.texture_map[&id]) == Some(tex) {
                            buffer.tint_val = 0.3;
                        }
                    }
                }
            }
            TreeValue::Thrusters(thruster_selection) => {
                let mut selected_bank = None;
                let mut selected_point = None;
                match thruster_selection {
                    ThrusterTreeValue::Bank(bank) => selected_bank = Some(bank),
                    ThrusterTreeValue::BankPoint(bank, point) => {
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
                            let mut radius = thruster_point.radius;
                            if hover_lollipop == Some(TreeValue::Thrusters(ThrusterTreeValue::BankPoint(bank_idx, point_idx))) {
                                radius = radius * 1.1 + 0.4
                            };
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
            TreeValue::Weapons(weapons_selection) => {
                let mut only_secondaries_displayed = false;
                let mut selected_bank = None;
                let mut selected_point = None;
                let mut selected_weapon_system = None;
                match weapons_selection {
                    WeaponTreeValue::PriBank(bank) => {
                        selected_bank = Some(bank);
                        selected_weapon_system = Some(&model.primary_weps);
                    }
                    WeaponTreeValue::SecBank(bank) => {
                        only_secondaries_displayed = true;
                        selected_bank = Some(bank);
                        selected_weapon_system = Some(&model.secondary_weps);
                    }
                    WeaponTreeValue::PriBankPoint(bank, point) => {
                        selected_bank = Some(bank);
                        selected_point = Some(point);
                        selected_weapon_system = Some(&model.primary_weps);
                    }
                    WeaponTreeValue::SecBankPoint(bank, point) => {
                        only_secondaries_displayed = true;
                        selected_bank = Some(bank);
                        selected_point = Some(point);
                        selected_weapon_system = Some(&model.secondary_weps);
                    }
                    WeaponTreeValue::PriHeader => {
                        selected_weapon_system = Some(&model.primary_weps);
                    }
                    WeaponTreeValue::SecHeader => {
                        only_secondaries_displayed = true;
                        selected_weapon_system = Some(&model.secondary_weps);
                    }
                    _ => {}
                }

                let weapon_system = if let Some(weapon_list) = selected_weapon_system {
                    weapon_list.iter().collect::<Vec<_>>()
                } else {
                    model.primary_weps.iter().chain(&model.secondary_weps).collect()
                };

                let primary_banks = self.model.primary_weps.len();

                const COLORS: [[f32; 4]; 3] = [LOLLIPOP_UNSELECTED_COLOR, LOLLIPOP_SELECTED_POINT_COLOR, LOLLIPOP_SELECTED_BANK_COLOR];
                self.lollipops = build_lollipops(
                    &COLORS,
                    display,
                    weapon_system.iter().enumerate().flat_map(|(bank_idx, weapon_bank)| {
                        weapon_bank.iter().enumerate().map(move |(point_idx, weapon_point)| {
                            let position = weapon_point.position;

                            // we're hovering a secondary if the bank is beyond the number of primary banks (because we're displaying everything and they were chained together)
                            // or we're only displaying secondaries in the first place
                            let hovered = (bank_idx >= primary_banks
                                && hover_lollipop == Some(TreeValue::Weapons(WeaponTreeValue::SecBankPoint(bank_idx - primary_banks, point_idx))))
                                || (only_secondaries_displayed
                                    && hover_lollipop == Some(TreeValue::Weapons(WeaponTreeValue::SecBankPoint(bank_idx, point_idx))))
                                || hover_lollipop == Some(TreeValue::Weapons(WeaponTreeValue::PriBankPoint(bank_idx, point_idx)));

                            let radius = if hovered {
                                model.header.max_radius * 0.06
                            } else {
                                model.header.max_radius * 0.03
                            };

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
            TreeValue::DockingBays(docking_selection) => {
                let selected_bank = if let DockingTreeValue::Bay(num) = docking_selection {
                    Some(num)
                } else {
                    None
                };

                const COLORS: [[f32; 4]; 3] = [LOLLIPOP_UNSELECTED_COLOR, LOLLIPOP_SELECTED_POINT_COLOR, LOLLIPOP_SELECTED_BANK_COLOR];
                self.lollipops = build_lollipops(
                    &COLORS,
                    display,
                    model.docking_bays.iter().enumerate().flat_map(|(bay_idx, docking_bay)| {
                        let position = docking_bay.position;
                        let mut radius = self.model.header.max_radius.powf(0.4) / 4.0;
                        if hover_lollipop == Some(TreeValue::DockingBays(DockingTreeValue::Bay(bay_idx))) {
                            radius *= 2.
                        };
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
            TreeValue::Glows(glow_selection) => {
                let mut selected_bank = None;
                let mut selected_point = None;
                match glow_selection {
                    GlowTreeValue::Bank(bank) => selected_bank = Some(bank),
                    GlowTreeValue::BankPoint(bank, point) => {
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
                            let mut radius = glow_point.radius * if enabled { 1.0 } else { 0.25 };
                            if hover_lollipop == Some(TreeValue::Glows(GlowTreeValue::BankPoint(bank_idx, point_idx))) {
                                radius *= 2.
                            };
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
            TreeValue::SpecialPoints(special_selection) => {
                let mut selected_point = None;
                if let SpecialPointTreeValue::Point(point) = special_selection {
                    selected_point = Some(point);
                }

                const COLORS: [[f32; 4]; 2] = [LOLLIPOP_SELECTED_BANK_COLOR, LOLLIPOP_SELECTED_POINT_COLOR];
                self.lollipops = build_lollipops(
                    &COLORS,
                    display,
                    model.special_points.iter().enumerate().map(|(point_idx, special_point)| {
                        let position = special_point.position;
                        let normal = Default::default(); // 0 vec
                        let mut radius = special_point.radius;
                        if hover_lollipop == Some(TreeValue::SpecialPoints(SpecialPointTreeValue::Point(point_idx))) {
                            radius = radius * 1.1 + 0.4
                        };
                        let selection = if selected_point == Some(point_idx) { SELECTED_POINT } else { UNSELECTED };
                        (position, normal, radius, selection)
                    }),
                );
            }
            TreeValue::Turrets(turret_selection) => {
                let mut selected_turret = None;
                let mut selected_point = None;
                match turret_selection {
                    TurretTreeValue::Turret(turret) => selected_turret = Some(turret),
                    TurretTreeValue::TurretPoint(turret, point) => {
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
                            let radius = if hover_lollipop == Some(TreeValue::Turrets(TurretTreeValue::TurretPoint(turret_idx, point_idx))) {
                                size * 2.
                            } else {
                                size
                            };

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
            TreeValue::Paths(path_selection) => {
                let mut selected_path = None;
                let mut selected_point = None;
                match path_selection {
                    PathTreeValue::Path(path) => selected_path = Some(path),
                    PathTreeValue::PathPoint(path, point) => {
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
                            let mut radius = path_point.radius;
                            if hover_lollipop == Some(TreeValue::Paths(PathTreeValue::PathPoint(path_idx, point_idx))) {
                                radius = radius * 1.1 + 0.4
                            };
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
            TreeValue::EyePoints(eye_selection) => {
                let mut selected_eye = None;
                if let EyeTreeValue::EyePoint(point) = eye_selection {
                    selected_eye = Some(point)
                }

                let size = 0.007 * model.header.max_radius;
                const COLORS: [[f32; 4]; 2] = [LOLLIPOP_SELECTED_BANK_COLOR, LOLLIPOP_SELECTED_POINT_COLOR];
                self.lollipops = build_lollipops(
                    &COLORS,
                    display,
                    model.eye_points.iter().enumerate().map(|(eye_idx, eye_point)| {
                        let position = eye_point.position;
                        let normal = eye_point.normal.0 * size * 2.0;
                        let radius = if hover_lollipop == Some(TreeValue::EyePoints(EyeTreeValue::EyePoint(eye_idx))) {
                            size * 2.
                        } else {
                            size
                        };

                        let selection = if selected_eye == Some(eye_idx) { SELECTED_POINT } else { UNSELECTED };
                        (position, normal, radius, selection)
                    }),
                );
            }
            TreeValue::VisualCenter => {
                let size = if hover_lollipop == Some(TreeValue::VisualCenter) {
                    0.04 * model.header.max_radius
                } else {
                    0.02 * model.header.max_radius
                };

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

fn closest_approach(line_a: Vec3d, line_b: Vec3d, point: Vec3d) -> Vec3d {
    let a2p = point - line_a;
    let a2b = line_b - line_a;

    let a2b_2 = a2b.magnitude_squared();
    let dot = a2p.dot(&a2b);

    let t = (dot) / (a2b_2);

    (a2b * t) + line_a
}

const LOLLIPOP_UNSELECTED_COLOR: [f32; 4] = [0.3, 0.3, 0.3, 0.15];
const LOLLIPOP_SELECTED_BANK_COLOR: [f32; 4] = [0.15, 0.15, 1.0, 0.15];
const LOLLIPOP_SELECTED_POINT_COLOR: [f32; 4] = [1.0, 0.15, 0.15, 0.15];

const UVEC_COLOR: [f32; 4] = [0.15, 0.15, 1.0, 0.15];
const FVEC_COLOR: [f32; 4] = [0.15, 1.0, 0.15, 0.15];

const LOLLIPOP_UNSELECTED_PATH_COLOR: [f32; 4] = [0.3, 0.3, 0.3, 0.005];
const LOLLIPOP_SELECTED_PATH_COLOR: [f32; 4] = [0.15, 0.15, 1.0, 0.05];
const LOLLIPOP_SELECTED_PATH_POINT_COLOR: [f32; 4] = [1.0, 0.15, 0.15, 0.1];

struct Graphics {
    circle_verts: VertexBuffer<Vertex>,
    circle_indices: IndexBuffer<u16>,
    square_verts: VertexBuffer<Vertex>,
    square_indices: IndexBuffer<u16>,
    box_verts: VertexBuffer<Vertex>,
    box_indices: IndexBuffer<u16>,
    icosphere_verts: VertexBuffer<Vertex>,
    icosphere_indices: IndexBuffer<u16>,
    arrowhead_verts: VertexBuffer<Vertex>,
    arrowhead_indices: IndexBuffer<u16>,
    frustum_verts: VertexBuffer<Vertex>,

    default_material_draw_params: glium::DrawParameters<'static>,
    arrowhead_draw_params: glium::DrawParameters<'static>,
    shield_draw_params: glium::DrawParameters<'static>,
    wireframe_params: glium::DrawParameters<'static>,
    lollipop_params: glium::DrawParameters<'static>,
    lollipop_stick_params: glium::DrawParameters<'static>,
    lollipop_rev_depth_params: glium::DrawParameters<'static>,
    drag_axis_params: glium::DrawParameters<'static>,
    orient_billboards_params: glium::DrawParameters<'static>,
    /// f, b, l, r, u, d
    orient_billboards: [SrgbTexture2d; 6],

    default_material_shader: glium::Program,
    textured_material_shader: glium::Program,
    flat_textured_material_shader: glium::Program,
    shield_shader: glium::Program,
    wireframe_shader: glium::Program,
    lollipop_stick_shader: glium::Program,
    lollipop_shader: glium::Program,
    arrowhead_shader: glium::Program,
    fov_shader: glium::Program,
}
impl Graphics {
    fn init(display: &Display) -> Self {
        fn load_img(display: &Display, bytes: &[u8]) -> SrgbTexture2d {
            let image = image::load(Cursor::new(bytes), image::ImageFormat::Png).unwrap().to_rgba8();
            let image_dimensions = image.dimensions();
            let image_raw = image.into_raw();
            SrgbTexture2d::new(display, glium::texture::RawImage2d::from_raw_rgba(image_raw, image_dimensions)).unwrap()
        }

        Graphics {
            circle_verts: glium::VertexBuffer::new(display, &*primitives::CIRCLE_VERTS).unwrap(),
            circle_indices: glium::IndexBuffer::new(display, glium::index::PrimitiveType::LineLoop, &primitives::CIRCLE_INDICES).unwrap(),
            square_verts: glium::VertexBuffer::new(display, &primitives::SQUARE_VERTS).unwrap(),
            square_indices: glium::IndexBuffer::new(display, glium::index::PrimitiveType::TrianglesList, &primitives::SQUARE_INDICES).unwrap(),
            box_verts: glium::VertexBuffer::new(display, &primitives::BOX_VERTS).unwrap(),
            box_indices: glium::IndexBuffer::new(display, glium::index::PrimitiveType::LinesList, &primitives::BOX_INDICES).unwrap(),
            icosphere_verts: glium::VertexBuffer::new(display, &primitives::SPHERE_VERTS).unwrap(),
            icosphere_indices: glium::IndexBuffer::new(display, glium::index::PrimitiveType::TrianglesList, &primitives::SPHERE_INDICES).unwrap(),
            arrowhead_verts: glium::VertexBuffer::new(display, &primitives::ARROWHEAD_VERTS).unwrap(),
            arrowhead_indices: glium::IndexBuffer::new(display, glium::index::PrimitiveType::TrianglesList, &primitives::ARROWHEAD_INDICES).unwrap(),
            frustum_verts: glium::VertexBuffer::new(display, &primitives::FRUSTUM_VERTS).unwrap(),
            default_material_draw_params: glium::DrawParameters {
                depth: glium::Depth {
                    test: glium::draw_parameters::DepthTest::IfLess,
                    write: true,
                    ..Default::default()
                },
                line_width: Some(1.0),
                backface_culling: glium::draw_parameters::BackfaceCullingMode::CullCounterClockwise,
                ..Default::default()
            },
            arrowhead_draw_params: glium::DrawParameters {
                depth: glium::Depth {
                    test: glium::draw_parameters::DepthTest::Overwrite,
                    write: false,
                    ..Default::default()
                },
                line_width: Some(1.0),
                backface_culling: glium::draw_parameters::BackfaceCullingMode::CullCounterClockwise,
                ..Default::default()
            },
            shield_draw_params: glium::DrawParameters {
                depth: glium::Depth {
                    test: glium::draw_parameters::DepthTest::IfLess,
                    write: true,
                    ..Default::default()
                },
                blend: glium::Blend::alpha_blending(),
                backface_culling: glium::draw_parameters::BackfaceCullingMode::CullCounterClockwise,
                ..Default::default()
            },
            wireframe_params: glium::DrawParameters {
                depth: glium::Depth {
                    test: glium::draw_parameters::DepthTest::Overwrite,
                    ..Default::default()
                },
                line_width: Some(1.0),
                backface_culling: glium::draw_parameters::BackfaceCullingMode::CullingDisabled,
                ..Default::default()
            },
            lollipop_params: glium::DrawParameters {
                depth: glium::Depth {
                    test: glium::draw_parameters::DepthTest::IfLess,
                    ..Default::default()
                },
                blend: ADDITIVE_BLEND,
                backface_culling: glium::draw_parameters::BackfaceCullingMode::CullingDisabled,
                ..Default::default()
            },
            lollipop_stick_params: glium::DrawParameters {
                depth: glium::Depth {
                    test: glium::draw_parameters::DepthTest::Overwrite,
                    write: false,
                    ..Default::default()
                },
                line_width: Some(2.0),
                backface_culling: glium::draw_parameters::BackfaceCullingMode::CullingDisabled,
                ..Default::default()
            },
            lollipop_rev_depth_params: glium::DrawParameters {
                depth: glium::Depth {
                    test: glium::draw_parameters::DepthTest::IfMore,
                    ..Default::default()
                },
                blend: ADDITIVE_BLEND,
                ..Default::default()
            },
            drag_axis_params: glium::DrawParameters {
                depth: glium::Depth {
                    test: glium::draw_parameters::DepthTest::IfLess,
                    write: false,
                    ..Default::default()
                },
                line_width: Some(2.0),
                ..Default::default()
            },
            orient_billboards_params: glium::DrawParameters {
                depth: glium::Depth {
                    test: glium::draw_parameters::DepthTest::IfLess,
                    write: false,
                    ..Default::default()
                },
                backface_culling: glium::draw_parameters::BackfaceCullingMode::CullCounterClockwise,
                ..Default::default()
            },
            orient_billboards: [
                load_img(display, include_bytes!("zforward.png")),
                load_img(display, include_bytes!("zbackward.png")),
                load_img(display, include_bytes!("xleft.png")),
                load_img(display, include_bytes!("xright.png")),
                load_img(display, include_bytes!("yup.png")),
                load_img(display, include_bytes!("ydown.png")),
            ],
            default_material_shader: glium::Program::from_source(display, DEFAULT_VERTEX_SHADER, DEFAULT_MAT_FRAGMENT_SHADER, None).unwrap(),
            textured_material_shader: glium::Program::from_source(display, DEFAULT_VERTEX_SHADER, TEXTURED_FRAGMENT_SHADER, None).unwrap(),
            flat_textured_material_shader: glium::Program::from_source(display, NO_NORMS_VERTEX_SHADER, FLAT_TEXTURED_FRAGMENT_SHADER, None).unwrap(),
            shield_shader: glium::Program::from_source(display, DEFAULT_VERTEX_SHADER, SHIELD_FRAGMENT_SHADER, None).unwrap(),
            wireframe_shader: glium::Program::from_source(display, NO_NORMS_VERTEX_SHADER, WIRE_FRAGMENT_SHADER, None).unwrap(),
            lollipop_stick_shader: glium::Program::from_source(display, NO_NORMS_VERTEX_SHADER, LOLLIPOP_STICK_FRAGMENT_SHADER, None).unwrap(),
            lollipop_shader: glium::Program::from_source(display, LOLLIPOP_VERTEX_SHADER, LOLLIPOP_FRAGMENT_SHADER, None).unwrap(),
            arrowhead_shader: glium::Program::from_source(display, NO_NORMS_VERTEX_SHADER, LOLLIPOP_FRAGMENT_SHADER, None).unwrap(),
            fov_shader: glium::Program::from_source(display, FOV_VERTEX_SHADER, LOLLIPOP_STICK_FRAGMENT_SHADER, None).unwrap(),
        }
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

const FOV_VERTEX_SHADER: &str = r#"
#version 140

in vec3 position;

uniform mat4 vert_matrix;
uniform vec3 world_offset;
uniform float scale;
uniform float fov;
uniform float max_fov;
uniform float base_fov;

void main() {
    float theta = fov + (max_fov - fov) * position.y;
    float phi = base_fov * position.x;
    vec3 pos = vec3(sin(theta) * sin(phi), cos(theta), sin(theta) * cos(phi)) * position.z;
    gl_Position = vert_matrix * vec4(pos * scale, 1.0);
}
"#;

const NO_NORMS_VERTEX_SHADER: &str = r#"
#version 140

in vec3 position;
in vec2 uv;

uniform mat4 vert_matrix;

out vec2 v_uv;

void main() {
    v_uv = uv;
    gl_Position = vert_matrix * vec4(position, 1.0);
}
"#;

const FLAT_TEXTURED_FRAGMENT_SHADER: &str = r#"
#version 140

in vec2 v_uv;

out vec4 color;

uniform sampler2D tex;

void main() {
    vec4 tex_color = texture(tex, v_uv);

    color = tex_color;
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
