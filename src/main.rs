//! Example how to use [epi::NativeTexture] with glium.
use native_dialog::FileDialog;
use std::{collections::HashMap, fs::File, sync::mpsc::TryRecvError};

//use egui::{FontFamily, TextStyle};
use glium::{glutin, BlendingFunction, Display, IndexBuffer, LinearBlendingFactor, VertexBuffer};
use pof::{Insignia, Model, ObjectId, Parser, ShieldData, SubObject, TextureId, Texturing, Vec3d};
extern crate nalgebra_glm as glm;

mod sphere;
mod ui;
use ui::{PofToolsGui, TreeSelection};

use crate::ui::{
    DockingSelection, EyeSelection, GlowSelection, InsigniaSelection, PathSelection, SpecialPointSelection, SubObjectSelection, TextureSelection,
    ThrusterSelection, TurretSelection, WeaponSelection,
};

use ui::Set::*;

fn create_display(event_loop: &glutin::event_loop::EventLoop<()>) -> glium::Display {
    let window_builder = glutin::window::WindowBuilder::new()
        .with_resizable(true)
        .with_inner_size(glutin::dpi::LogicalSize { width: 800.0, height: 600.0 })
        .with_title("Pof Tools");

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

        self.stick_vertices.push(Vertex { position: vertex.to_tuple() });
        self.stick_vertices.push(Vertex { position: (vertex + normal).to_tuple() });
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
        let mut vertices = vec![];
        let mut normals = vec![];
        let mut indices = vec![];

        for poly in &shield_data.polygons {
            vertices.push(Vertex {
                position: shield_data.verts[poly.verts.0 .0 as usize].to_tuple(),
            });
            normals.push(Normal { normal: poly.normal.to_tuple() });
            indices.push(indices.len() as u32);

            vertices.push(Vertex {
                position: shield_data.verts[poly.verts.1 .0 as usize].to_tuple(),
            });
            normals.push(Normal { normal: poly.normal.to_tuple() });
            indices.push(indices.len() as u32);

            vertices.push(Vertex {
                position: shield_data.verts[poly.verts.2 .0 as usize].to_tuple(),
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
        let mut vertices = vec![];
        let mut normals = vec![];
        let mut indices = vec![];

        for (vert1, vert2, vert3) in &insignia.faces {
            let [v1, v2, v3] = [vert1, vert2, vert3].map(|i| nalgebra_glm::Vec3::from(insignia.vertices[i.vertex_id.0 as usize]));
            let normal: Vec3d = (v2 - v1).cross(&(v3 - v1)).normalize().into();

            vertices.push(Vertex {
                position: insignia.vertices[vert1.vertex_id.0 as usize].to_tuple(),
            });
            normals.push(Normal { normal: normal.to_tuple() });
            indices.push(indices.len() as u32);

            vertices.push(Vertex {
                position: insignia.vertices[vert2.vertex_id.0 as usize].to_tuple(),
            });
            normals.push(Normal { normal: normal.to_tuple() });
            indices.push(indices.len() as u32);

            vertices.push(Vertex {
                position: insignia.vertices[vert3.vertex_id.0 as usize].to_tuple(),
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

struct GlBufferedObject {
    obj_id: ObjectId,
    tmap: TextureId,
    vertices: VertexBuffer<Vertex>,
    normals: VertexBuffer<Normal>,
    indices: IndexBuffer<u32>,
    tint_val: f32,
}
impl GlBufferedObject {
    // TODO DONT TRINAGULATE FOR WIREFRAMES
    fn new(display: &Display, object: &SubObject, tmap: TextureId) -> Option<GlBufferedObject> {
        let mut vertices = vec![];
        let mut normals = vec![];
        let mut indices = vec![];

        let mut map = HashMap::new();

        let bsp_data = &object.bsp_data;

        for (_, poly_list) in bsp_data.collision_tree.leaves() {
            for poly in poly_list {
                if let Texturing::Texture(tex) = poly.texture {
                    if tex != tmap {
                        continue;
                    }
                }

                // need to triangulate possibly
                // we'll make tris like this 0,1,2 .. 0,2,3 .. 0,3,4... etc
                if let [first, remainder @ ..] = &*poly.verts {
                    // split the first vertex off from the rest of them
                    for vertpair in remainder.windows(2) {
                        // for each pair of verts in the remainder make a tri of them, [first, vertpair[0], vertpair[1]],
                        let index = *map.entry((first.vertex_id, first.normal_id)).or_insert_with(|| {
                            let n = vertices.len();
                            vertices.push(Vertex {
                                position: bsp_data.verts[first.vertex_id.0 as usize].to_tuple(),
                            });
                            normals.push(Normal {
                                normal: bsp_data.norms[first.normal_id.0 as usize].to_tuple(),
                            });
                            n.try_into().unwrap()
                        });
                        indices.push(index);

                        for polyvert in vertpair {
                            let index = *map.entry((polyvert.vertex_id, polyvert.normal_id)).or_insert_with(|| {
                                let n = vertices.len();
                                vertices.push(Vertex {
                                    position: bsp_data.verts[polyvert.vertex_id.0 as usize].to_tuple(),
                                });
                                normals.push(Normal {
                                    normal: bsp_data.norms[polyvert.normal_id.0 as usize].to_tuple(),
                                });
                                n.try_into().unwrap()
                            });
                            indices.push(index);
                        }
                    }
                }
            }
        }

        if vertices.is_empty() {
            None
        } else {
            Some(GlBufferedObject {
                obj_id: object.obj_id,
                tmap,
                vertices: glium::VertexBuffer::new(display, &vertices).unwrap(),
                normals: glium::VertexBuffer::new(display, &normals).unwrap(),
                indices: glium::IndexBuffer::new(display, glium::index::PrimitiveType::TrianglesList, &indices).unwrap(),
                tint_val: 0.0,
            })
        }
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
}

glium::implement_vertex!(Vertex, position);

#[derive(Copy, Clone)]
pub struct Normal {
    normal: (f32, f32, f32),
}

glium::implement_vertex!(Normal, normal);

impl PofToolsGui {
    fn save_model(model: &Model) {
        // use a scoped thread here, its ok to block the main window for now i guess
        crossbeam::thread::scope(|s| {
            s.spawn(|_| {
                let path = FileDialog::new().add_filter("Parallax Object File", &["pof"]).show_save_single_file();
                println!("{:#?}", path);
                if let Ok(path) = path {
                    if let Some(path) = path {
                        let mut file = File::create(path).unwrap();
                        println!("{:#?}", file);

                        model.write(&mut file).unwrap();
                    }
                }
            });
        })
        .unwrap();
    }

    // opens a thread which opens the dialog and starts parsing a model
    fn start_loading_model(&mut self) {
        let (sender, receiver) = std::sync::mpsc::channel();
        self.loading_thread = Some(receiver);

        std::thread::spawn(move || {
            let path = FileDialog::new()
                .add_filter("All supported files", &["pof", "dae"])
                .add_filter("COLLADA", &["dae"])
                .add_filter("Parallax Object File", &["pof"])
                .show_open_single_file()
                .unwrap();

            if let Some(path) = path {
                let ext = path.extension().map(|ext| ext.to_ascii_lowercase());
                let model = match ext.as_ref().and_then(|ext| ext.to_str()) {
                    Some("dae") => pof::parse_dae(path),
                    Some("pof") => {
                        let file = File::open(path).unwrap();
                        let mut parser = Parser::new(file).unwrap();
                        Box::new(parser.parse().unwrap())
                    }
                    _ => todo!(),
                };
                // println!("{:#?}", model);
                let _ = sender.send(Some(model));
            } else {
                let _ = sender.send(None);
            }
        });
    }

    // after the above thread has returned, stuffs the new model in
    fn finish_loading_model(&mut self, display: &Display) {
        self.buffer_objects.clear();
        self.buffer_shield = None;
        self.buffer_insignias.clear();

        for subobject in &self.model.sub_objects {
            for i in 0..self.model.textures.len() {
                let buf = GlBufferedObject::new(&display, &subobject, TextureId(i as u32));
                if let Some(buf) = buf {
                    self.buffer_objects.push(buf);
                }
            }
        }

        for insignia in &self.model.insignias {
            self.buffer_insignias.push(GlBufferedInsignia::new(&display, &insignia));
        }

        if let Some(shield) = &self.model.shield_data {
            self.buffer_shield = Some(GlBufferedShield::new(&display, &shield));
        }

        self.maybe_recalculate_3d_helpers(&display);

        self.warnings.clear();
        PofToolsGui::recheck_warnings(&mut self.warnings, &self.model, All);
        self.ui_state.tree_view_selection = Default::default();
        self.ui_state.refresh_properties_panel(&self.model);
        self.camera_heading = 2.7;
        self.camera_pitch = -0.4;
        self.camera_scale = self.model.header.max_radius * 2.0;
    }
}

fn main() {
    let event_loop = glutin::event_loop::EventLoop::with_user_event();
    let mut pt_gui = PofToolsGui::default();
    let display = create_display(&event_loop);

    let mut egui = egui_glium::EguiGlium::new(&display);

    pt_gui.start_loading_model();
    egui.ctx().output().cursor_icon = egui::CursorIcon::Wait;

    let model = &pt_gui.model;

    // lots of graphics stuff to initialize
    let default_material_shader = glium::Program::from_source(&display, DEFAULT_VERTEX_SHADER, DEFAULT_MAT_FRAGMENT_SHADER, None).unwrap();
    let shield_shader = glium::Program::from_source(&display, DEFAULT_VERTEX_SHADER, SHIELD_FRAGMENT_SHADER, None).unwrap();
    let wireframe_shader = glium::Program::from_source(&display, DEFAULT_VERTEX_SHADER, WIRE_FRAGMENT_SHADER, None).unwrap();
    let lollipop_stick_shader = glium::Program::from_source(&display, DEFAULT_VERTEX_SHADER, LOLLIPOP_STICK_FRAGMENT_SHADER, None).unwrap();
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

    let box_verts = glium::VertexBuffer::new(&display, &sphere::BOX_VERTS).unwrap();
    let box_indices = glium::IndexBuffer::new(&display, glium::index::PrimitiveType::LinesList, &sphere::BOX_INDICES).unwrap();

    let icosphere_verts = glium::VertexBuffer::new(&display, &sphere::SPHERE_VERTS).unwrap();
    let icosphere_indices = glium::IndexBuffer::new(&display, glium::index::PrimitiveType::TrianglesList, &sphere::SPHERE_INDICES).unwrap();

    pt_gui.camera_heading = 2.7;
    pt_gui.camera_pitch = -0.4;
    pt_gui.camera_scale = model.header.max_radius * 2.0;

    event_loop.run(move |event, _, control_flow| {
        let mut redraw = || {
            if let Some(thread) = &pt_gui.loading_thread {
                let response = thread.try_recv();
                match response {
                    Ok(Some(data)) => {
                        pt_gui.model = data;
                        pt_gui.finish_loading_model(&display);

                        pt_gui.loading_thread = None;

                        egui.ctx().output().cursor_icon = egui::CursorIcon::Default;
                    }

                    Err(TryRecvError::Disconnected) => {
                        pt_gui.loading_thread = None;
                    }

                    Ok(None) => {
                        pt_gui.loading_thread = None;
                    }

                    Err(TryRecvError::Empty) => {}
                }
            }

            egui.begin_frame(&display);

            pt_gui.show_ui(egui.ctx());

            let (needs_repaint, shapes) = egui.end_frame(&display);

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

                // handle user interactions like rotating the camera
                let rect = egui.ctx().available_rect();
                let input = egui.ctx().input();
                if rect.is_positive() {
                    let mouse_pos = input.pointer.hover_pos();
                    let last_click_pos = input.pointer.press_origin();
                    let in_3d_viewport = mouse_pos.map_or(false, |hover_pos| rect.contains(hover_pos));
                    let clicked_in_3d_viewport = last_click_pos.map_or(false, |hover_pos| rect.contains(hover_pos));
                    if clicked_in_3d_viewport && input.pointer.button_down(egui::PointerButton::Secondary) {
                        pt_gui.camera_heading += input.pointer.delta().x * -0.01;
                        pt_gui.camera_pitch += input.pointer.delta().y * -0.01;
                    }
                    if in_3d_viewport {
                        pt_gui.camera_scale *= 1.0 + (input.scroll_delta.y * -0.001)
                    }
                }

                // set up the camera matrix
                let perspective_matrix = {
                    let (width, height) = target.get_dimensions();
                    let aspect_ratio = height as f32 / width as f32;

                    let fov: f32 = 3.141592 / 3.0;
                    let zfar = (model.header.max_radius + pt_gui.camera_scale) * 2.0;
                    let znear = (model.header.max_radius + pt_gui.camera_scale) / 1000.;

                    let f = 1.0 / (fov / 2.0).tan();

                    [
                        [f * aspect_ratio, 0.0, 0.0, 0.0],
                        [0.0, f, 0.0, 0.0],
                        [0.0, 0.0, (zfar + znear) / (zfar - znear), 1.0],
                        [0.0, 0.0, -(2.0 * zfar * znear) / (zfar - znear), 0.0],
                    ]
                };

                let mut view_mat = glm::translation(&glm::vec3(0.0, 0.0, pt_gui.camera_scale));

                let light_vec = glm::vec3(0.5, 1.0, -1.0);

                view_mat *= glm::rotation(pt_gui.camera_pitch, &glm::vec3(1., 0., 0.)); // pitch
                view_mat *= glm::rotation(pt_gui.camera_heading, &glm::vec3(0., 1., 0.)); // heading
                view_mat.prepend_translation_mut(&glm::vec3(-model.auto_center.x, -model.auto_center.y, -model.auto_center.z));

                let view_mat: [[f32; 4]; 4] = view_mat.into(); // the final matrix used by the graphics

                let displayed_subobjects = get_list_of_display_subobjects(model, &pt_gui.ui_state.tree_view_selection);

                // brighten up the dark bits so wireframe is easier to see
                let dark_color;
                if pt_gui.wireframe_enabled {
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
                    | TreeSelection::AutoCenter => light_color = [0.3, 0.3, 0.3f32],
                    _ => light_color = [0.9, 0.9, 0.9f32],
                }

                // draw the actual subobjects of the model
                for buffer_obj in &pt_gui.buffer_objects {
                    if displayed_subobjects[buffer_obj.obj_id.0 as usize] {
                        let mut mat = glm::identity::<f32, 4>();
                        mat.append_translation_mut(&pt_gui.model.get_total_subobj_offset(buffer_obj.obj_id).into());

                        // only render if its currently being displayed
                        let uniforms = glium::uniform! {
                            model: <[[f32; 4]; 4]>::from(mat),
                            view: view_mat,
                            perspective: perspective_matrix,
                            u_light: <[f32; 3]>::from(light_vec),
                            dark_color: dark_color,
                            light_color: light_color,
                            tint_color: [0.0, 0.0, 1.0f32],
                            tint_val: buffer_obj.tint_val,
                        };

                        target
                            .draw(
                                (&buffer_obj.vertices, &buffer_obj.normals),
                                &buffer_obj.indices,
                                &default_material_shader,
                                &uniforms,
                                &default_material_draw_params,
                            )
                            .unwrap();
                    }
                }

                // maybe draw the insignias
                if let TreeSelection::Insignia(insignia_select) = &pt_gui.tree_view_selection {
                    let (current_detail_level, current_insignia_idx) = match *insignia_select {
                        InsigniaSelection::Header => (0, None),
                        InsigniaSelection::Insignia(idx) => (pt_gui.model.insignias[idx].detail_level, Some(idx)),
                    };

                    for (i, insig_buffer) in pt_gui.buffer_insignias.iter().enumerate() {
                        let insignia = &pt_gui.model.insignias[i];
                        if insignia.detail_level == current_detail_level {
                            let mut mat = glm::identity::<f32, 4>();
                            mat.append_translation_mut(&insignia.offset.into());

                            let color;
                            if current_insignia_idx == Some(i) {
                                color = [1.0, 0.0, 0.0f32];
                            } else {
                                color = [0.0, 0.0, 1.0f32];
                            }

                            // only render if its currently being displayed
                            let uniforms = glium::uniform! {
                                model: <[[f32; 4]; 4]>::from(mat),
                                view: view_mat,
                                perspective: perspective_matrix,
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
                        let uniforms = glium::uniform! {
                            model: <[[f32; 4]; 4]>::from(glm::identity::<f32, 4>()),
                            view: view_mat,
                            perspective: perspective_matrix,
                            u_light: [0.0, 0.0, -1.0f32],
                            dark_color: [0.0, 0.0, 0.0f32],
                            light_color: [0.2, 0.3, 0.9f32],
                            tint_color: [0.0, 0.0, 1.0f32],
                            tint_val: 0.0f32,
                        };

                        target
                            .draw((&shield.vertices, &shield.normals), &shield.indices, &shield_shader, &uniforms, &shield_draw_params)
                            .unwrap();
                    }
                }

                // draw 'helpers'
                // bounding boxes, lollipops, etc
                match &pt_gui.ui_state.tree_view_selection {
                    &TreeSelection::SubObjects(SubObjectSelection::SubObject(obj_id)) => {
                        // draw wireframe bounding boxes
                        let bbox = &pt_gui.model.sub_objects[obj_id.0 as usize].bbox;

                        let mut mat = glm::scaling(&(bbox.max - bbox.min).into());
                        mat.append_translation_mut(&(bbox.min + pt_gui.model.get_total_subobj_offset(obj_id)).into());
                        let uniforms = glium::uniform! {
                            model: <[[f32; 4]; 4]>::from(mat),
                            view: view_mat,
                            perspective: perspective_matrix,
                        };

                        for buffer in &pt_gui.buffer_objects {
                            if buffer.obj_id == obj_id {
                                target
                                    .draw(&box_verts, &box_indices, &wireframe_shader, &uniforms, &wireframe_params)
                                    .unwrap();
                            }
                        }
                    }
                    TreeSelection::Thrusters(_)
                    | TreeSelection::Weapons(_)
                    | TreeSelection::DockingBays(_)
                    | TreeSelection::Glows(_)
                    | TreeSelection::SpecialPoints(_)
                    | TreeSelection::Turrets(_)
                    | TreeSelection::Paths(_)
                    | TreeSelection::EyePoints(_)
                    | TreeSelection::AutoCenter => {
                        // draw lollipops!
                        for lollipop_group in &pt_gui.lollipops {
                            if let TreeSelection::Paths(_) = &pt_gui.ui_state.tree_view_selection {
                                lollipop_params.blend = glium::Blend::alpha_blending();
                            } else {
                                lollipop_params.blend = ADDITIVE_BLEND;
                            }

                            let uniforms = glium::uniform! {
                                model: <[[f32; 4]; 4]>::from(glm::identity::<f32, 4>()),
                                view: view_mat,
                                perspective: perspective_matrix,
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
                                let uniforms = glium::uniform! {
                                    model: <[[f32; 4]; 4]>::from(glm::identity::<f32, 4>()),
                                    view: view_mat,
                                    perspective: perspective_matrix,
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
                    _ => {}
                }

                egui.paint(&display, &mut target, shapes);

                target.finish().unwrap();
            }
        };
        //println!("{:#?}", event);
        match event {
            // Platform-dependent event handlers to workaround a winit bug
            // See: https://github.com/rust-windowing/winit/issues/987
            // See: https://github.com/rust-windowing/winit/issues/1619
            glutin::event::Event::RedrawEventsCleared if cfg!(windows) => redraw(),
            glutin::event::Event::RedrawRequested(_) if !cfg!(windows) => redraw(),
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
                if egui.is_quit_event(&event) {
                    *control_flow = glium::glutin::event_loop::ControlFlow::Exit;
                }

                egui.on_event(&event);

                display.gl_window().window().request_redraw(); // TODO: ask egui if the events warrants a repaint instead
            }

            _ => (),
        }
    });
}

// based on the current selection which submodels should be displayed
// TODO show destroyed models
fn get_list_of_display_subobjects(model: &Model, tree_selection: &TreeSelection) -> Vec<bool> {
    let mut out = vec![false; model.sub_objects.len()];

    // if they have something selected...
    if let TreeSelection::SubObjects(SubObjectSelection::SubObject(selected_id)) = *tree_selection {
        // first lets see if they a have a detail level (or a child of it) selected
        let mut detail_selected = None;
        for detail_level in &model.header.detail_levels {
            if model.is_obj_id_ancestor(selected_id, *detail_level) {
                detail_selected = Some(*detail_level);
            }
        }

        // if so, display it and all its children
        if let Some(detail_level) = detail_selected {
            for (i, sub_object) in model.sub_objects.iter().enumerate() {
                out[i] = model.is_obj_id_ancestor(sub_object.obj_id, detail_level) && !sub_object.is_destroyed_model();
            }
        } else if model.sub_objects[selected_id.0 as usize].is_debris_model {
            // if they have debris selected show all the debris
            for (i, sub_object) in model.sub_objects.iter().enumerate() {
                out[i] = sub_object.is_debris_model;
            }
        } else {
            out[selected_id.0 as usize] = true
        }
    } else if let TreeSelection::Insignia(InsigniaSelection::Insignia(idx)) = *tree_selection {
        // show the LOD objects according to the detail level of the currently selected insignia
        for (i, sub_object) in model.sub_objects.iter().enumerate() {
            out[i] = model.is_obj_id_ancestor(sub_object.obj_id, model.header.detail_levels[model.insignias[idx].detail_level as usize])
                && !sub_object.is_destroyed_model();
        }
    } else {
        // initialize to all the detail0 children by default
        for (i, sub_object) in model.sub_objects.iter().enumerate() {
            if model.header.detail_levels.is_empty() {
                // wut, no detail levels? ok just show only the currently selected one...
                out[i] = sub_object.obj_id == ObjectId(0);
            } else {
                let detail0 = model.header.detail_levels[0];
                out[i] = model.is_obj_id_ancestor(sub_object.obj_id, detail0) && !sub_object.is_destroyed_model();
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

        if !self.ui_state.viewport_3d_dirty {
            return;
        }

        for buffer in self.buffer_objects.iter_mut() {
            buffer.tint_val = 0.0;
        }

        self.lollipops.clear();

        let model = &self.model;

        const UNSELECTED: usize = 0;
        const SELECTED_POINT: usize = 1;
        const SELECTED_BANK: usize = 2;

        // determine what bank/point is selected, if any
        // push the according lollipop positions/normals based on the points positions/normals
        // push into 3 separate vectors, for 3 separate colors depending on selection state
        match &self.ui_state.tree_view_selection {
            TreeSelection::SubObjects(SubObjectSelection::SubObject(obj_id)) => {
                for buffer in &mut self.buffer_objects {
                    if buffer.obj_id == *obj_id {
                        buffer.tint_val = 0.2;
                    }
                }
            }
            TreeSelection::Textures(TextureSelection::Texture(tex)) => {
                for buffer in &mut self.buffer_objects {
                    if buffer.tmap == *tex {
                        buffer.tint_val = 0.3;
                    }
                }
            }
            TreeSelection::Thrusters(thruster_selection) => {
                let mut selected_bank = None;
                let mut selected_point = None;
                match *thruster_selection {
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
                    &display,
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
                match *weapons_selection {
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
                    &display,
                    weapon_system.iter().enumerate().flat_map(|(bank_idx, weapon_bank)| {
                        weapon_bank.iter().enumerate().map(move |(point_idx, weapon_point)| {
                            let position = weapon_point.position;
                            let radius = model.header.max_radius * 0.03;
                            let normal = weapon_point.normal * radius * 2.0;
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
                let mut selected_bank = None;
                let mut selected_point = None;
                match *docking_selection {
                    DockingSelection::Bay(bank) => selected_bank = Some(bank),
                    DockingSelection::BayPoint(bank, point) => {
                        selected_bank = Some(bank);
                        selected_point = Some(point);
                    }
                    _ => {}
                }

                const COLORS: [[f32; 4]; 3] = [LOLLIPOP_UNSELECTED_COLOR, LOLLIPOP_SELECTED_POINT_COLOR, LOLLIPOP_SELECTED_BANK_COLOR];
                self.lollipops = build_lollipops(
                    &COLORS,
                    &display,
                    model.docking_bays.iter().enumerate().flat_map(|(bay_idx, docking_bay)| {
                        docking_bay.points.iter().enumerate().map(move |(point_idx, docking_point)| {
                            let position = docking_point.position;
                            let radius = 1.0;
                            let normal = docking_point.normal * radius * 2.0;
                            let selection = if selected_bank == Some(bay_idx) {
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
            TreeSelection::Glows(glow_selection) => {
                let mut selected_bank = None;
                let mut selected_point = None;
                match *glow_selection {
                    GlowSelection::Bank(bank) => selected_bank = Some(bank),
                    GlowSelection::BankPoint(bank, point) => {
                        selected_bank = Some(bank);
                        selected_point = Some(point);
                    }
                    _ => {}
                }

                const COLORS: [[f32; 4]; 3] = [LOLLIPOP_UNSELECTED_COLOR, LOLLIPOP_SELECTED_POINT_COLOR, LOLLIPOP_SELECTED_BANK_COLOR];
                self.lollipops = build_lollipops(
                    &COLORS,
                    &display,
                    model.glow_banks.iter().enumerate().flat_map(|(bank_idx, glow_bank)| {
                        glow_bank.glow_points.iter().enumerate().map(move |(point_idx, glow_point)| {
                            let position = glow_point.position;
                            let normal = glow_point.normal * glow_point.radius * 2.0;
                            let radius = glow_point.radius;
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
                    selected_point = Some(*point);
                }

                const COLORS: [[f32; 4]; 2] = [LOLLIPOP_SELECTED_BANK_COLOR, LOLLIPOP_SELECTED_POINT_COLOR];
                self.lollipops = build_lollipops(
                    &COLORS,
                    &display,
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
                match *turret_selection {
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
                    &display,
                    model.turrets.iter().enumerate().flat_map(|(turret_idx, turret)| {
                        let offset = self.model.get_total_subobj_offset(turret.gun_obj);
                        turret.fire_points.iter().enumerate().map(move |(point_idx, fire_point)| {
                            let position = *fire_point + offset;
                            let normal = turret.normal * size * 2.0;
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
                match *path_selection {
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
                    &display,
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
                match *eye_selection {
                    EyeSelection::EyePoint(point) => selected_eye = Some(point),
                    _ => {}
                }

                let size = 0.007 * model.header.max_radius;

                const COLORS: [[f32; 4]; 2] = [LOLLIPOP_SELECTED_BANK_COLOR, LOLLIPOP_SELECTED_POINT_COLOR];
                self.lollipops = build_lollipops(
                    &COLORS,
                    &display,
                    model.eye_points.iter().enumerate().map(|(eye_idx, eye_point)| {
                        let position = eye_point.offset;
                        let normal = eye_point.normal * size * 2.0;
                        let radius = size;
                        let selection = if selected_eye == Some(eye_idx) { SELECTED_POINT } else { UNSELECTED };
                        (position, normal, radius, selection)
                    }),
                );
            }
            TreeSelection::AutoCenter => {
                let size = 0.02 * model.header.max_radius;

                let mut lollipop_origin = GlLollipopsBuilder::new(LOLLIPOP_SELECTED_BANK_COLOR);
                lollipop_origin.push(Vec3d::ZERO, Vec3d::ZERO, size);
                let lollipop_origin = lollipop_origin.finish(&display);

                let mut lollipop_auto_center = GlLollipopsBuilder::new(LOLLIPOP_SELECTED_POINT_COLOR);
                lollipop_auto_center.push(model.auto_center, Vec3d::ZERO, size);
                let lollipop_auto_center = lollipop_auto_center.finish(&display);

                self.lollipops = vec![lollipop_origin, lollipop_auto_center];
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

out vec3 v_normal;

uniform mat4 perspective;
uniform mat4 view;
uniform mat4 model;

void main() {
    mat4 modelview = view * model;
    v_normal = transpose(inverse(mat3(modelview))) * normal;
    gl_Position = perspective * modelview * vec4(position, 1.0);
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
in vec3 normal;

out vec3 v_normal;

uniform mat4 perspective;
uniform mat4 view;
uniform mat4 model;

void main() {
    mat4 modelview = view * model;
    v_normal = transpose(inverse(mat3(modelview))) * normal;
    gl_Position = perspective * modelview * world_matrix * vec4(position, 1.0);
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
