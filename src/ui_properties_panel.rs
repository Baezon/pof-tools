use std::str::FromStr;

use egui::{style::Widgets, text::LayoutJob, CollapsingHeader, Color32, DragValue, Label, Response, RichText, TextFormat, TextStyle, Ui};
use glium::Display;
use nalgebra_glm::TMat4;
use pof::{
    Dock, EyePoint, GlowPoint, GlowPointBank, Insignia, Model, ObjectId, PathId, PathPoint, SpecialPoint, SubsysMovementAxis, ThrusterGlow, Vec3d,
    WeaponHardpoint,
};

use crate::{
    ui::{
        DockingSelection, Error, EyeSelection, GlowSelection, InsigniaSelection, PathSelection, PofToolsGui, Set::*, SpecialPointSelection,
        SubObjectSelection, TextureSelection, ThrusterSelection, TreeSelection, TurretSelection, UiState, Warning, WeaponSelection,
    },
    GlBufferedObject,
};

enum IndexingButtonsResponse {
    Switch(usize),
    Copy(usize),
    Delete(usize),
    Push,
}
impl IndexingButtonsResponse {
    fn apply<T: Clone + Default>(self, data_vec: &mut Vec<T>) -> Option<usize> {
        match self {
            IndexingButtonsResponse::Switch(idx) => {
                assert!(idx < data_vec.len());
                Some(idx)
            }
            IndexingButtonsResponse::Copy(idx) => {
                assert!(idx < data_vec.len());
                let new_idx = data_vec.len();
                let item = data_vec[idx].clone();
                data_vec.push(item);
                Some(new_idx)
            }
            IndexingButtonsResponse::Delete(idx) => {
                assert!(idx < data_vec.len());
                data_vec.remove(idx);
                if idx < data_vec.len() {
                    Some(idx)
                } else {
                    idx.checked_sub(1)
                }
            }
            IndexingButtonsResponse::Push => {
                let new_idx = data_vec.len();
                data_vec.push(Default::default());
                Some(new_idx)
            }
        }
    }
}

impl UiState {
    fn set_widget_color(ui: &mut Ui, color: Color32) {
        ui.visuals_mut().widgets.hovered.fg_stroke.color = color;
        ui.visuals_mut().widgets.inactive.fg_stroke.color = color;
        ui.visuals_mut().widgets.active.fg_stroke.color = color;
        ui.visuals_mut().widgets.open.fg_stroke.color = color;
    }

    fn reset_widget_color(ui: &mut Ui) {
        ui.visuals_mut().widgets = Widgets::default();
    }

    #[must_use]
    fn list_manipulator_widget(
        ui: &mut Ui, current_num: Option<usize>, list_len: Option<usize>, index_name: &str,
    ) -> Option<IndexingButtonsResponse> {
        enum Icon {
            Arrow,
            Plus,
        }

        // figuring out what actions the left/right buttons should be is not simple...
        let (left, right) = match (list_len, current_num) {
            (None, None) => (None, None),
            (None, Some(_)) => unreachable!(),
            (Some(list_len), None) => (None, Some(if list_len == 0 { Icon::Plus } else { Icon::Arrow })),
            (Some(list_len), Some(num)) => {
                (if num == 0 { None } else { Some(Icon::Arrow) }, Some(if num + 1 == list_len { Icon::Plus } else { Icon::Arrow }))
            }
        };
        let mut ret = None;

        ui.horizontal(|ui| {
            let side_button_size = 40.0;
            let height = 50.0;
            let remaining_width = ui.available_width() - ui.spacing().item_spacing.x * 2.0 - side_button_size * 2.0;

            // the left/previous item button
            ui.add_enabled_ui(left.is_some(), |ui| {
                if ui
                    .add_sized([side_button_size, height], egui::Button::new("◀"))
                    .on_hover_text(format!("Switch to the previous {}", index_name))
                    .clicked()
                {
                    ret = Some(IndexingButtonsResponse::Switch(current_num.unwrap() - 1));
                }
            });

            // the copy/delete buttons (and label)
            ui.vertical(|ui| {
                ui.add_sized(
                    [remaining_width, (height - ui.spacing().item_spacing.y) / 2.0],
                    egui::Label::new(current_num.map_or_else(|| "-".to_string(), |num| format!("{} {}", index_name, num + 1))),
                );

                ui.add_enabled_ui(current_num.is_some(), |ui| {
                    ui.horizontal(|ui| {
                        if ui
                            .add_sized(
                                [
                                    (remaining_width - ui.spacing().item_spacing.x) / 2.0,
                                    (height - ui.spacing().item_spacing.y) / 2.0,
                                ],
                                egui::Button::new("🗐"),
                            )
                            .on_hover_text(format!("Copy this {} into a new {}", index_name, index_name))
                            .clicked()
                            && list_len.is_some()
                        {
                            ret = Some(IndexingButtonsResponse::Copy(current_num.unwrap()));
                        }
                        if ui
                            .add_sized(
                                [
                                    (remaining_width - ui.spacing().item_spacing.x) / 2.0,
                                    (height - ui.spacing().item_spacing.y) / 2.0,
                                ],
                                egui::Button::new("🗑"),
                            )
                            .on_hover_text(format!("Delete this {}", index_name))
                            .clicked()
                            && list_len.is_some()
                        {
                            ret = Some(IndexingButtonsResponse::Delete(current_num.unwrap()));
                        }
                    });
                });
            });

            // the right/new button
            ui.add_enabled_ui(right.is_some(), |ui| {
                if matches!(right, Some(Icon::Plus)) {
                    if ui
                        .add_sized([side_button_size, height], egui::Button::new("✚"))
                        .on_hover_text(format!("Add a new {}", index_name))
                        .clicked()
                    {
                        ret = Some(IndexingButtonsResponse::Push);
                    }
                } else if ui
                    .add_sized([side_button_size, height], egui::Button::new("▶"))
                    .on_hover_text(format!("Switch to the next {}", index_name))
                    .clicked()
                {
                    ret = Some(IndexingButtonsResponse::Switch(current_num.map_or(0, |num| num + 1)));
                }
            });
        });
        ret
    }

    // a text edit field attached to a model value that will show up red if it cannot parse
    fn parsable_text_edit<T: FromStr>(ui: &mut Ui, model_value: &mut T, parsable_string: &mut String) -> bool {
        if parsable_string.parse::<T>().is_err() {
            ui.visuals_mut().override_text_color = Some(Color32::RED);
        }
        if ui.text_edit_singleline(parsable_string).changed() {
            if let Ok(value) = parsable_string.parse() {
                *model_value = value;
                return true;
            }
        }
        false
    }

    // a combo box for subobjects
    // selector value is a convenience option to disable the combo box, since most properties panels work on a current selection of Option<something>
    fn subobject_combo_box<T>(
        ui: &mut Ui, name_list: &[String], mut_selection: &mut usize, selector_value: Option<T>, label: &str, active_error_idx: Option<usize>,
    ) -> Option<usize> {
        let mut ret = None;

        ui.add_enabled_ui(selector_value.is_some(), |ui| {
            if selector_value.is_some() {
                let color = if active_error_idx.is_some() {
                    UiState::set_widget_color(ui, Color32::RED);
                    Color32::RED
                } else {
                    ui.visuals().text_color()
                };
                egui::ComboBox::from_label(RichText::new(label).color(color))
                    .width(100.0)
                    .selected_text(name_list[*mut_selection].clone())
                    .show_ui(ui, |ui| {
                        for (i, name) in name_list.iter().enumerate() {
                            if active_error_idx == Some(i) {
                                // change the color of this entry to red if its the error one
                                ui.visuals_mut().override_text_color = Some(color);
                            }
                            if ui.selectable_value(mut_selection, i, name).clicked() {
                                ret = Some(*mut_selection);
                            }
                            ui.visuals_mut().override_text_color = None;
                        }
                    });

                UiState::reset_widget_color(ui);
            } else {
                egui::ComboBox::from_label(label).show_index(ui, mut_selection, 1, |_| format!(""));
            }
        });
        ret
    }

    fn model_value_edit<T: FromStr>(
        viewport_3d_dirty: &mut bool, ui: &mut Ui, active_warning: bool, model_value: Option<&mut T>, parsable_string: &mut String,
    ) -> Response {
        if let Some(value) = model_value {
            if parsable_string.parse::<T>().is_err() {
                ui.visuals_mut().override_text_color = Some(Color32::RED);
            } else if active_warning {
                ui.visuals_mut().override_text_color = Some(Color32::YELLOW);
            }
            let response = ui.text_edit_singleline(parsable_string);
            if response.changed() {
                if let Ok(parsed_string) = parsable_string.parse() {
                    *value = parsed_string;
                    *viewport_3d_dirty = true;
                }
            }
            ui.visuals_mut().override_text_color = None;
            response
        } else {
            ui.add_enabled_ui(false, |ui| ui.text_edit_singleline(parsable_string)).inner
        }
    }

    fn show_transform_window(ctx: &egui::Context, transform_window: &mut TransformWindow) -> Option<TMat4<f32>> {
        let mut ret = None;
        let window = egui::Window::new("Transform")
            .collapsible(false)
            .resizable(false)
            .default_size((250.0, 200.0))
            .open(&mut transform_window.open)
            .anchor(egui::Align2::RIGHT_TOP, [-100.0, 100.0]);

        window.show(ctx, |ui| {
            let mut changed = false;
            ui.horizontal(|ui| {
                changed = changed
                    || ui
                        .selectable_value(&mut transform_window.transform_type, TransformType::Rotate, "Rotate")
                        .clicked();
                ui.separator();
                changed = changed
                    || ui
                        .selectable_value(&mut transform_window.transform_type, TransformType::Scale, "Scale")
                        .clicked();
                ui.separator();
                changed = changed
                    || ui
                        .selectable_value(&mut transform_window.transform_type, TransformType::Translate, "Translate")
                        .clicked();
            });
            if changed {
                transform_window.axis_select = 0;
                transform_window.vector = format!("1, 0, 0");
                transform_window.value = format!("1");
            }
            ui.separator();

            let mut mat = glm::identity::<f32, 4>();
            let mut valid_input = false;

            match transform_window.transform_type {
                TransformType::Rotate => {
                    ui.horizontal(|ui| {
                        if ui.button("X-axis").clicked() {
                            transform_window.vector = format!("1, 0, 0");
                        }
                        ui.separator();
                        if ui.button("Y-axis").clicked() {
                            transform_window.vector = format!("0, 1, 0");
                        }
                        ui.separator();
                        if ui.button("Z-axis").clicked() {
                            transform_window.vector = format!("0, 0, 1");
                        }
                        ui.separator();
                    });
                    ui.separator();
                    ui.label("Axis:");

                    if transform_window.vector.parse::<Vec3d>().is_err() {
                        ui.visuals_mut().override_text_color = Some(Color32::RED);
                    }
                    ui.text_edit_singleline(&mut transform_window.vector);
                    ui.visuals_mut().override_text_color = None;

                    ui.label("Angle:");
                    if transform_window.value.parse::<f32>().is_err() {
                        ui.visuals_mut().override_text_color = Some(Color32::RED);
                    }
                    ui.text_edit_singleline(&mut transform_window.value);
                    ui.visuals_mut().override_text_color = None;

                    if let Ok(vector) = transform_window.vector.parse::<Vec3d>() {
                        if let Ok(angle) = transform_window.value.parse::<f32>() {
                            mat = glm::rotation(angle.to_radians(), &vector.into());
                            valid_input = true;
                        }
                    }
                }
                TransformType::Scale => {
                    ui.horizontal(|ui| {
                        ui.selectable_value(&mut transform_window.axis_select, 0, "All axes");
                        ui.separator();
                        ui.selectable_value(&mut transform_window.axis_select, 1, "X-axis");
                        ui.separator();
                        ui.selectable_value(&mut transform_window.axis_select, 2, "Y-axis");
                        ui.separator();
                        ui.selectable_value(&mut transform_window.axis_select, 3, "Z-axis");
                        ui.separator();
                    });
                    ui.separator();
                    ui.label("Scalar (negative values flip):");
                    if transform_window.value.parse::<f32>().is_err() {
                        ui.visuals_mut().override_text_color = Some(Color32::RED);
                    }
                    ui.text_edit_singleline(&mut transform_window.value);
                    ui.visuals_mut().override_text_color = None;

                    if let Ok(scalar) = transform_window.value.parse::<f32>() {
                        let vector = match transform_window.axis_select {
                            0 => glm::vec3(scalar, scalar, scalar),
                            1 => glm::vec3(scalar, 1.0, 1.0),
                            2 => glm::vec3(1.0, scalar, 1.0),
                            3 => glm::vec3(1.0, 1.0, scalar),
                            _ => unreachable!(),
                        };
                        mat = glm::scaling(&vector);
                        valid_input = true;
                    }
                }
                TransformType::Translate => {
                    ui.label("Translation Vector:");

                    if transform_window.vector.parse::<Vec3d>().is_err() {
                        ui.visuals_mut().override_text_color = Some(Color32::RED);
                    }
                    ui.text_edit_singleline(&mut transform_window.vector);
                    ui.visuals_mut().override_text_color = None;

                    if let Ok(vector) = transform_window.vector.parse::<Vec3d>() {
                        mat = glm::translation(&vector.into());
                        valid_input = true;
                    }
                }
            }

            ui.separator();
            if ui.add_enabled(valid_input, egui::Button::new("Apply")).clicked() {
                ret = Some(mat);
            }
        });

        ret
    }

    // fills the properties panel based on the current tree selection, taking all the relevant data from the model
    pub(crate) fn refresh_properties_panel(&mut self, model: &Model) {
        match self.tree_view_selection {
            TreeSelection::Header => {
                self.properties_panel = PropertiesPanel::Header {
                    bbox_min_string: format!("{}", model.header.bbox.min),
                    bbox_max_string: format!("{}", model.header.bbox.max),
                    radius_string: format!("{}", model.header.max_radius),
                    mass_string: format!("{}", model.header.mass),
                    moir_string: format!(
                        "{:e}, {:e}, {:e}",
                        model.header.moment_of_inertia.rvec.x, model.header.moment_of_inertia.rvec.y, model.header.moment_of_inertia.rvec.z
                    ),
                    moiu_string: format!(
                        "{:e}, {:e}, {:e}",
                        model.header.moment_of_inertia.uvec.x, model.header.moment_of_inertia.uvec.y, model.header.moment_of_inertia.uvec.z
                    ),
                    moif_string: format!(
                        "{:e}, {:e}, {:e}",
                        model.header.moment_of_inertia.fvec.x, model.header.moment_of_inertia.fvec.y, model.header.moment_of_inertia.fvec.z
                    ),
                    transform_window: Default::default(),
                }
            }
            TreeSelection::SubObjects(subobj_tree_select) => match subobj_tree_select {
                SubObjectSelection::Header => self.properties_panel = PropertiesPanel::default_subobject(),
                SubObjectSelection::SubObject(id) => {
                    self.properties_panel = PropertiesPanel::SubObject {
                        bbox_max_string: format!("{}", model.sub_objects[id].bbox.max),
                        bbox_min_string: format!("{}", model.sub_objects[id].bbox.min),
                        offset_string: format!("{}", model.sub_objects[id].offset),
                        radius_string: format!("{}", model.sub_objects[id].radius),
                        is_debris_check: model.sub_objects[id].is_debris_model,
                        name: format!("{}", model.sub_objects[id].name),
                        rot_axis: model.sub_objects[id].movement_axis,
                        transform_window: Default::default(),
                    }
                }
            },
            TreeSelection::Textures(tex_tree_select) => match tex_tree_select {
                TextureSelection::Header => self.properties_panel = PropertiesPanel::default_texture(),
                TextureSelection::Texture(texture_id) => {
                    self.properties_panel = PropertiesPanel::Texture {
                        texture_name: format!("{}", model.textures[texture_id.0 as usize]),
                    }
                }
            },
            TreeSelection::Thrusters(thruster_tree_select) => match thruster_tree_select {
                ThrusterSelection::Header => self.properties_panel = PropertiesPanel::default_thruster(),
                ThrusterSelection::Bank(bank) => {
                    self.properties_panel = PropertiesPanel::Thruster {
                        engine_subsys_string: format!(
                            "{}",
                            pof::properties_get_field(&model.thruster_banks[bank].properties, "$engine_subsystem").unwrap_or_default()
                        ),
                        radius_string: Default::default(),
                        normal_string: Default::default(),
                        position_string: Default::default(),
                    }
                }
                ThrusterSelection::BankPoint(bank, point) => {
                    self.properties_panel = PropertiesPanel::Thruster {
                        engine_subsys_string: format!(
                            "{}",
                            pof::properties_get_field(&model.thruster_banks[bank].properties, "$engine_subsystem").unwrap_or_default()
                        ),
                        radius_string: format!("{}", model.thruster_banks[bank].glows[point].radius),
                        normal_string: format!("{}", model.thruster_banks[bank].glows[point].normal),
                        position_string: format!("{}", model.thruster_banks[bank].glows[point].position),
                    }
                }
            },
            TreeSelection::Weapons(weapons_tree_select) => match weapons_tree_select {
                WeaponSelection::PriBankPoint(bank_idx, point_idx) => {
                    self.properties_panel = PropertiesPanel::Weapon {
                        position_string: format!("{}", model.primary_weps[bank_idx][point_idx].position),
                        normal_string: format!("{}", model.primary_weps[bank_idx][point_idx].normal),
                        offset_string: format!("{}", model.primary_weps[bank_idx][point_idx].offset),
                    }
                }
                WeaponSelection::SecBankPoint(bank_idx, point_idx) => {
                    self.properties_panel = PropertiesPanel::Weapon {
                        position_string: format!("{}", model.secondary_weps[bank_idx][point_idx].position),
                        normal_string: format!("{}", model.secondary_weps[bank_idx][point_idx].normal),
                        offset_string: format!("{}", model.secondary_weps[bank_idx][point_idx].offset),
                    }
                }
                _ => self.properties_panel = PropertiesPanel::default_weapon(),
            },
            TreeSelection::DockingBays(docking_select) => match docking_select {
                DockingSelection::Bay(bay) => {
                    self.properties_panel = PropertiesPanel::DockingBay {
                        name_string: pof::properties_get_field(&model.docking_bays[bay].properties, "$name")
                            .map_or(format!("Dock {}", bay + 1), |name| format!("{}", name)),
                        position_string: format!("{}", model.docking_bays[bay].position),
                        fvec_string: format!("{}", model.docking_bays[bay].fvec.0),
                        uvec_ang: model.docking_bays[bay].get_uvec_angle().to_degrees() % 360.0,
                        path_num: model.docking_bays[bay].path.unwrap_or(PathId(model.paths.len() as u32)).0 as usize,
                    }
                }
                _ => self.properties_panel = PropertiesPanel::default_docking_bay(),
            },
            TreeSelection::Glows(glow_select) => match glow_select {
                GlowSelection::BankPoint(bank, point) => {
                    self.properties_panel = PropertiesPanel::GlowBank {
                        disp_time_string: format!("{}", model.glow_banks[bank].disp_time),
                        on_time_string: format!("{}", model.glow_banks[bank].on_time),
                        off_time_string: format!("{}", model.glow_banks[bank].off_time),
                        attached_subobj_idx: model.glow_banks[bank].obj_parent.0 as usize,
                        lod_string: format!("{}", model.glow_banks[bank].lod),
                        glow_type_string: format!("{}", model.glow_banks[bank].glow_type),
                        glow_texture_string: format!(
                            "{}",
                            pof::properties_get_field(&model.glow_banks[bank].properties, "$glow_texture").unwrap_or_default()
                        ),
                        position_string: format!("{}", model.glow_banks[bank].glow_points[point].position),
                        normal_string: format!("{}", model.glow_banks[bank].glow_points[point].normal),
                        radius_string: format!("{}", model.glow_banks[bank].glow_points[point].radius),
                    }
                }
                GlowSelection::Bank(bank) => {
                    self.properties_panel = PropertiesPanel::GlowBank {
                        disp_time_string: format!("{}", model.glow_banks[bank].disp_time),
                        on_time_string: format!("{}", model.glow_banks[bank].on_time),
                        off_time_string: format!("{}", model.glow_banks[bank].off_time),
                        attached_subobj_idx: model.glow_banks[bank].obj_parent.0 as usize,
                        lod_string: format!("{}", model.glow_banks[bank].lod),
                        glow_type_string: format!("{}", model.glow_banks[bank].glow_type),
                        glow_texture_string: format!(
                            "{}",
                            pof::properties_get_field(&model.glow_banks[bank].properties, "$glow_texture").unwrap_or_default()
                        ),
                        position_string: Default::default(),
                        normal_string: Default::default(),
                        radius_string: Default::default(),
                    }
                }
                _ => self.properties_panel = PropertiesPanel::default_glow(),
            },
            TreeSelection::SpecialPoints(special_select) => match special_select {
                SpecialPointSelection::Point(point) => {
                    self.properties_panel = PropertiesPanel::SpecialPoint {
                        name_string: format!("{}", model.special_points[point].name),
                        position_string: format!("{}", model.special_points[point].position),
                        radius_string: format!("{}", model.special_points[point].radius),
                    }
                }
                _ => self.properties_panel = PropertiesPanel::default_special_point(),
            },
            TreeSelection::Turrets(turret_selection) => match turret_selection {
                TurretSelection::TurretPoint(turret, point) => {
                    self.properties_panel = PropertiesPanel::Turret {
                        normal_string: format!("{}", model.turrets[turret].normal),
                        base_idx: model.turrets[turret].base_obj.0 as usize,
                        position_string: format!("{}", model.turrets[turret].fire_points[point]),
                    }
                }
                TurretSelection::Turret(turret) => {
                    self.properties_panel = PropertiesPanel::Turret {
                        normal_string: format!("{}", model.turrets[turret].normal),
                        base_idx: model.turrets[turret].base_obj.0 as usize,
                        position_string: Default::default(),
                    }
                }
                _ => self.properties_panel = PropertiesPanel::default_turret(),
            },
            TreeSelection::Paths(path_selection) => match path_selection {
                PathSelection::PathPoint(path, point) => {
                    self.properties_panel = PropertiesPanel::Path {
                        name: format!("{}", model.paths[path].name),
                        parent_string: format!("{}", model.paths[path].parent),
                        position_string: format!("{}", model.paths[path].points[point].position),
                        radius_string: format!("{}", model.paths[path].points[point].radius),
                    }
                }
                PathSelection::Path(path) => {
                    self.properties_panel = PropertiesPanel::Path {
                        name: format!("{}", model.paths[path].name),
                        parent_string: format!("{}", model.paths[path].parent),
                        position_string: Default::default(),
                        radius_string: Default::default(),
                    }
                }
                _ => self.properties_panel = PropertiesPanel::default_path(),
            },
            TreeSelection::Insignia(insig_selection) => match insig_selection {
                InsigniaSelection::Insignia(idx) => {
                    self.properties_panel = PropertiesPanel::Insignia {
                        lod_string: format!("{}", model.insignias[idx].detail_level),
                        offset_string: format!("{}", model.insignias[idx].offset),
                    }
                }
                _ => self.properties_panel = PropertiesPanel::default_insignia(),
            },
            TreeSelection::EyePoints(eye_selection) => match eye_selection {
                EyeSelection::EyePoint(idx) => {
                    self.properties_panel = PropertiesPanel::EyePoint {
                        position_string: format!("{}", model.eye_points[idx].offset),
                        normal_string: format!("{}", model.eye_points[idx].normal),
                        attached_subobj_idx: model.eye_points[idx].attached_subobj.0 as usize,
                    }
                }
                _ => self.properties_panel = PropertiesPanel::default_eye(),
            },
            TreeSelection::Shield => self.properties_panel = PropertiesPanel::Shield, // nothing mutable to refresh! woohoo!'
            TreeSelection::VisualCenter => self.properties_panel = PropertiesPanel::VisualCenter { position: format!("{}", model.visual_center) },
            TreeSelection::Comments => self.properties_panel = PropertiesPanel::Comments,
        }
    }
}

#[derive(Default)]
pub(crate) struct TransformWindow {
    open: bool,
    vector: String,
    value: String,
    axis_select: usize,
    transform_type: TransformType,
}

#[derive(PartialEq)]
enum TransformType {
    Rotate,
    Scale,
    Translate,
}
impl Default for TransformType {
    fn default() -> Self {
        Self::Rotate
    }
}

pub(crate) enum PropertiesPanel {
    Header {
        bbox_min_string: String,
        bbox_max_string: String,
        radius_string: String,
        mass_string: String,
        moir_string: String,
        moiu_string: String,
        moif_string: String,
        transform_window: TransformWindow,
    },
    SubObject {
        bbox_min_string: String,
        bbox_max_string: String,
        name: String,
        offset_string: String,
        radius_string: String,
        is_debris_check: bool,
        rot_axis: SubsysMovementAxis,
        transform_window: TransformWindow,
    },
    Texture {
        texture_name: String,
    },
    Thruster {
        engine_subsys_string: String,
        normal_string: String,
        position_string: String,
        radius_string: String,
    },
    Weapon {
        position_string: String,
        normal_string: String,
        offset_string: String,
    },
    DockingBay {
        name_string: String,
        position_string: String,
        fvec_string: String,
        uvec_ang: f32,
        path_num: usize,
    },
    GlowBank {
        disp_time_string: String,
        on_time_string: String,
        off_time_string: String,
        attached_subobj_idx: usize,
        lod_string: String,
        glow_type_string: String,
        glow_texture_string: String,
        position_string: String,
        normal_string: String,
        radius_string: String,
    },
    SpecialPoint {
        name_string: String,
        position_string: String,
        radius_string: String,
    },
    Turret {
        base_idx: usize,
        normal_string: String,
        position_string: String,
    },
    Path {
        name: String,
        parent_string: String,
        position_string: String,
        radius_string: String,
    },
    Insignia {
        lod_string: String,
        offset_string: String,
    },
    Shield,
    EyePoint {
        position_string: String,
        normal_string: String,
        attached_subobj_idx: usize,
    },
    VisualCenter {
        position: String,
    },
    Comments,
}
impl Default for PropertiesPanel {
    fn default() -> Self {
        PropertiesPanel::Header {
            bbox_min_string: Default::default(),
            bbox_max_string: Default::default(),
            radius_string: Default::default(),
            mass_string: Default::default(),
            moir_string: Default::default(),
            moiu_string: Default::default(),
            moif_string: Default::default(),
            transform_window: TransformWindow {
                open: false,
                vector: format!("1, 0, 0"),
                value: format!("1"),
                axis_select: 0,
                transform_type: TransformType::Rotate,
            },
        }
    }
}
impl PropertiesPanel {
    fn default_subobject() -> Self {
        Self::SubObject {
            bbox_min_string: Default::default(),
            bbox_max_string: Default::default(),
            name: Default::default(),
            offset_string: Default::default(),
            radius_string: Default::default(),
            is_debris_check: Default::default(),
            rot_axis: SubsysMovementAxis::NONE,
            transform_window: TransformWindow {
                open: false,
                vector: format!("1, 0, 0"),
                value: format!("1"),
                axis_select: 0,
                transform_type: TransformType::Rotate,
            },
        }
    }
    fn default_texture() -> Self {
        Self::Texture { texture_name: Default::default() }
    }
    fn default_thruster() -> Self {
        Self::Thruster {
            engine_subsys_string: Default::default(),
            radius_string: Default::default(),
            normal_string: Default::default(),
            position_string: Default::default(),
        }
    }
    fn default_weapon() -> Self {
        Self::Weapon {
            position_string: Default::default(),
            normal_string: Default::default(),
            offset_string: Default::default(),
        }
    }
    fn default_docking_bay() -> Self {
        Self::DockingBay {
            name_string: Default::default(),
            position_string: Default::default(),
            fvec_string: Default::default(),
            uvec_ang: Default::default(),
            path_num: Default::default(),
        }
    }
    fn default_glow() -> Self {
        Self::GlowBank {
            position_string: Default::default(),
            disp_time_string: Default::default(),
            on_time_string: Default::default(),
            off_time_string: Default::default(),
            attached_subobj_idx: Default::default(),
            lod_string: Default::default(),
            glow_texture_string: Default::default(),
            glow_type_string: Default::default(),
            normal_string: Default::default(),
            radius_string: Default::default(),
        }
    }
    fn default_special_point() -> Self {
        Self::SpecialPoint {
            radius_string: Default::default(),
            name_string: Default::default(),
            position_string: Default::default(),
        }
    }
    fn default_turret() -> Self {
        Self::Turret {
            base_idx: Default::default(),
            normal_string: Default::default(),
            position_string: Default::default(),
        }
    }
    fn default_path() -> Self {
        Self::Path {
            name: Default::default(),
            parent_string: Default::default(),
            position_string: Default::default(),
            radius_string: Default::default(),
        }
    }
    fn default_eye() -> Self {
        Self::EyePoint {
            position_string: Default::default(),
            normal_string: Default::default(),
            attached_subobj_idx: 0,
        }
    }
    fn default_insignia() -> Self {
        Self::Insignia {
            lod_string: Default::default(),
            offset_string: Default::default(),
        }
    }
}

impl PofToolsGui {
    pub(crate) fn do_properties_panel(&mut self, ui: &mut egui::Ui, ctx: &egui::Context, display: &Display) {
        let mut reload_textures = false;
        let mut properties_panel_dirty = false;

        // this is needed for blank string fields when the properties panel can't display
        // anything for that field due to an invalid tree selection
        let mut blank_string = String::new();

        match &mut self.ui_state.properties_panel {
            PropertiesPanel::Header {
                bbox_min_string,
                bbox_max_string,
                mass_string,
                radius_string,
                moir_string,
                moiu_string,
                moif_string,
                transform_window,
            } => {
                ui.heading("Header");
                ui.separator();

                let mut bbox_changed = false;
                let mut display_bbox = false;
                ui.horizontal(|ui| {
                    ui.label("Bounding Box:");
                    let response = ui.button("Recalculate");

                    if response.clicked() {
                        self.model.recalc_bbox();
                        properties_panel_dirty = true;
                        bbox_changed = true;
                    }
                    display_bbox = response.hovered() || response.has_focus() || display_bbox;
                });

                ui.horizontal(|ui| {
                    ui.label("Min:");
                    let response = UiState::model_value_edit(
                        &mut self.ui_state.viewport_3d_dirty,
                        ui,
                        self.warnings.contains(&Warning::BBoxTooSmall(None)),
                        Some(&mut self.model.header.bbox.min),
                        bbox_min_string,
                    );

                    if response.changed() {
                        bbox_changed = true;
                    }
                    display_bbox = response.hovered() || response.has_focus() || display_bbox;
                });

                ui.horizontal(|ui| {
                    ui.label("Max:");
                    let response = UiState::model_value_edit(
                        &mut self.ui_state.viewport_3d_dirty,
                        ui,
                        self.warnings.contains(&Warning::BBoxTooSmall(None)),
                        Some(&mut self.model.header.bbox.max),
                        bbox_max_string,
                    );

                    if response.changed() {
                        bbox_changed = true;
                    }
                    self.ui_state.display_bbox = response.hovered() || response.has_focus() || display_bbox;
                });

                let mut radius_changed = false;
                let mut display_radius = false;
                ui.horizontal(|ui| {
                    ui.add(egui::Label::new("Radius:"));
                    let response = ui.button("Recalculate");

                    if response.clicked() {
                        self.model.recalc_radius();
                        radius_changed = true;
                        properties_panel_dirty = true;
                    }
                    display_radius = response.hovered() || response.has_focus() || display_radius;
                });

                let response = UiState::model_value_edit(
                    &mut self.ui_state.viewport_3d_dirty,
                    ui,
                    self.warnings.contains(&Warning::RadiusTooSmall(None)),
                    Some(&mut self.model.header.max_radius),
                    radius_string,
                );

                if response.changed() {
                    radius_changed = true;
                }
                self.ui_state.display_radius = response.hovered() || response.has_focus() || display_radius;

                ui.horizontal(|ui| {
                    ui.add(egui::Label::new("Mass:"));
                    if ui.button("Recalculate").clicked() {
                        self.model.recalc_mass();
                        properties_panel_dirty = true;
                    }
                });
                UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, Some(&mut self.model.header.mass), mass_string);

                ui.horizontal(|ui| {
                    ui.add(egui::Label::new("Moment of Inertia:"));
                    if ui.button("Recalculate").clicked() {
                        self.model.recalc_moi();
                        properties_panel_dirty = true;
                    }
                });
                UiState::model_value_edit(
                    &mut self.ui_state.viewport_3d_dirty,
                    ui,
                    false,
                    Some(&mut self.model.header.moment_of_inertia.rvec),
                    moir_string,
                );
                UiState::model_value_edit(
                    &mut self.ui_state.viewport_3d_dirty,
                    ui,
                    false,
                    Some(&mut self.model.header.moment_of_inertia.uvec),
                    moiu_string,
                );
                UiState::model_value_edit(
                    &mut self.ui_state.viewport_3d_dirty,
                    ui,
                    false,
                    Some(&mut self.model.header.moment_of_inertia.fvec),
                    moif_string,
                );

                if radius_changed {
                    PofToolsGui::recheck_warnings(&mut self.warnings, &self.model, One(Warning::RadiusTooSmall(None)));
                }
                if bbox_changed {
                    PofToolsGui::recheck_warnings(&mut self.warnings, &self.model, One(Warning::BBoxTooSmall(None)));
                }

                ui.separator();

                if ui.add(egui::Button::new("Transform Mesh")).clicked() {
                    transform_window.open = true;
                }
                if let Some(matrix) = UiState::show_transform_window(ctx, transform_window) {
                    for i in 0..self.model.sub_objects.len() {
                        // only apply to top-level subobjects (no parent), apply_transform() will
                        // recursively apply the proper transform to its children
                        if self.model.sub_objects[ObjectId(i as u32)].parent() == None {
                            self.model.apply_transform(ObjectId(i as u32), &matrix, true);
                            self.ui_state.viewport_3d_dirty = true;
                            properties_panel_dirty = true;

                            for buf in &mut self.buffer_objects {
                                if buf.obj_id == ObjectId(i as u32) || self.model.is_obj_id_ancestor(buf.obj_id, ObjectId(i as u32)) {
                                    let new_buf = GlBufferedObject::new(display, &self.model.sub_objects[buf.obj_id], buf.texture_id);
                                    if let Some(new_buf) = new_buf {
                                        *buf = new_buf;
                                    }
                                }
                            }
                        }
                    }

                    self.model.recalc_bbox();
                    self.model.recalc_radius();
                }
            }
            PropertiesPanel::SubObject {
                bbox_min_string,
                bbox_max_string,
                name,
                offset_string,
                radius_string,
                is_debris_check,
                rot_axis,
                transform_window,
            } => {
                ui.heading("SubObject");
                ui.separator();

                let selected_id = if let TreeSelection::SubObjects(SubObjectSelection::SubObject(id)) = self.ui_state.tree_view_selection {
                    Some(id)
                } else {
                    None
                };

                ui.label("Name:");
                if let Some(id) = selected_id {
                    ui.add(egui::TextEdit::singleline(&mut self.model.sub_objects[id].name));
                } else {
                    ui.add_enabled(false, egui::TextEdit::singleline(name));
                }

                ui.add_space(5.0);

                let num_debris = self.model.num_debris_objects();
                let cannot_be_debris =
                    num_debris >= pof::MAX_DEBRIS_OBJECTS || selected_id.map_or(false, |id| self.model.header.detail_levels.contains(&id));

                ui.add_enabled_ui(
                    selected_id.is_some() && (self.model.sub_objects[selected_id.unwrap()].is_debris_model || !cannot_be_debris),
                    |ui| {
                        if selected_id.map_or(false, |id| self.model.sub_objects[id].is_debris_model)
                            && (self.model.header.detail_levels.contains(&selected_id.unwrap()) || num_debris > pof::MAX_DEBRIS_OBJECTS)
                        {
                            UiState::set_widget_color(ui, Color32::RED);
                        }

                        let mut checkbox = ui.checkbox(is_debris_check, "Debris Subobject");

                        if selected_id.map_or(false, |_| num_debris >= pof::MAX_DEBRIS_OBJECTS) {
                            checkbox = checkbox.on_disabled_hover_text(format!("The maximum number of debris is {}", pof::MAX_DEBRIS_OBJECTS));
                        }

                        if selected_id.map_or(false, |id| self.model.header.detail_levels.contains(&id)) {
                            checkbox = checkbox.on_disabled_hover_text(format!("A detail object cannot also be debris"));
                        }

                        if checkbox.changed() {
                            self.model.sub_objects[selected_id.unwrap()].is_debris_model = *is_debris_check;
                            PofToolsGui::recheck_errors(&mut self.errors, &self.model, One(Error::TooManyDebrisObjects));
                            PofToolsGui::recheck_errors(&mut self.errors, &self.model, One(Error::DetailAndDebrisObj(selected_id.unwrap())));
                        }

                        UiState::reset_widget_color(ui);
                    },
                );

                ui.add_space(5.0);

                let mut bbox_changed = false;
                let mut display_bbox = false;
                ui.horizontal(|ui| {
                    ui.label("Bounding Box:");
                    let response = ui.add_enabled(selected_id.is_some(), egui::Button::new("Recalculate"));

                    if response.clicked() {
                        self.model.sub_objects[selected_id.unwrap()].recalc_bbox();
                        properties_panel_dirty = true;
                        bbox_changed = true;
                    }
                    display_bbox = response.hovered() || response.has_focus() || display_bbox;
                });

                ui.horizontal(|ui| {
                    ui.label("Min:");
                    let response = UiState::model_value_edit(
                        &mut self.ui_state.viewport_3d_dirty,
                        ui,
                        false,
                        selected_id.map(|id| &mut self.model.sub_objects[id].bbox.min),
                        bbox_min_string,
                    );

                    if response.changed() {
                        bbox_changed = true;
                    }
                    display_bbox = response.hovered() || response.has_focus() || display_bbox;
                });

                ui.horizontal(|ui| {
                    ui.label("Max:");
                    let response = UiState::model_value_edit(
                        &mut self.ui_state.viewport_3d_dirty,
                        ui,
                        false,
                        selected_id.map(|id| &mut self.model.sub_objects[id].bbox.max),
                        bbox_max_string,
                    );

                    if response.changed() {
                        bbox_changed = true;
                    }
                    display_bbox = response.hovered() || response.has_focus() || display_bbox;
                });

                self.ui_state.display_bbox = display_bbox;

                if bbox_changed {
                    PofToolsGui::recheck_warnings(&mut self.warnings, &self.model, One(Warning::BBoxTooSmall(selected_id)));
                }

                ui.label("Offset:");
                let response = UiState::model_value_edit(
                    &mut self.ui_state.viewport_3d_dirty,
                    ui,
                    false,
                    selected_id.map(|id| &mut self.model.sub_objects[id].offset),
                    offset_string,
                );

                self.ui_state.display_origin = response.hovered() || response.has_focus();

                let mut radius_changed = false;
                let mut display_radius = false;
                ui.horizontal(|ui| {
                    ui.label("Radius:");
                    let response = ui.add_enabled(selected_id.is_some(), egui::Button::new("Recalculate"));

                    if response.clicked() {
                        self.model.sub_objects[selected_id.unwrap()].recalc_radius();
                        properties_panel_dirty = true;
                        radius_changed = true;
                    }
                    display_radius = response.hovered() || response.has_focus() || display_radius;
                });

                let response = UiState::model_value_edit(
                    &mut self.ui_state.viewport_3d_dirty,
                    ui,
                    self.warnings.contains(&Warning::RadiusTooSmall(selected_id)),
                    selected_id.map(|id| &mut self.model.sub_objects[id].radius),
                    radius_string,
                );

                if response.changed() {
                    radius_changed = true;
                }
                self.ui_state.display_radius = response.hovered() || response.has_focus() || display_radius;

                if radius_changed {
                    PofToolsGui::recheck_warnings(&mut self.warnings, &self.model, One(Warning::RadiusTooSmall(selected_id)));
                }

                ui.separator();

                if ui
                    .add_enabled(matches!(selected_id, Some(_)), egui::Button::new("Transform Mesh"))
                    .clicked()
                {
                    transform_window.open = true;
                }
                if let Some(matrix) = UiState::show_transform_window(ctx, transform_window) {
                    if let Some(id) = selected_id {
                        self.model.apply_transform(id, &matrix, false);
                        self.ui_state.viewport_3d_dirty = true;
                        properties_panel_dirty = true;

                        for buf in &mut self.buffer_objects {
                            if buf.obj_id == id || self.model.is_obj_id_ancestor(buf.obj_id, id) {
                                let new_buf = GlBufferedObject::new(display, &self.model.sub_objects[buf.obj_id], buf.texture_id);
                                if let Some(new_buf) = new_buf {
                                    *buf = new_buf;
                                }
                            }
                        }
                    }
                }

                // first index is none
                let mut subobj_names_list = vec![format!("None")];
                let mut combo_idx = 0;
                // add the current parent if it exists as the second entry
                if let Some(parent_id) = selected_id.and_then(|id| self.model.sub_objects[id].parent()) {
                    subobj_names_list.push(self.model.sub_objects[parent_id].name.clone());
                    combo_idx = 1;
                }

                // fill the remainder with the rest of the subobjects which are NOT children of this one
                if let Some(id) = selected_id {
                    subobj_names_list.extend(
                        self.model
                            .sub_objects
                            .iter()
                            .filter(|subobj| {
                                self.model.sub_objects[id].parent() != Some(subobj.obj_id) && !self.model.is_obj_id_ancestor(subobj.obj_id, id)
                            })
                            .map(|subobj| subobj.name.clone()),
                    );
                }

                if let Some(new_parent) = UiState::subobject_combo_box(ui, &subobj_names_list, &mut combo_idx, selected_id, "Parent", None) {
                    self.model.make_orphan(selected_id.unwrap());

                    if new_parent != 0 {
                        let parent_id = self.model.get_obj_id_by_name(&subobj_names_list[new_parent]).unwrap();
                        self.model.make_parent(parent_id, selected_id.unwrap());
                    }

                    //Error::InvalidTurretGunSubobject(())
                    //Error::DetailObjWithParent(())
                    PofToolsGui::recheck_errors(&mut self.errors, &self.model, All);
                }

                ui.label("Properties:");
                if let Some(id) = selected_id {
                    ui.add(egui::TextEdit::multiline(&mut self.model.sub_objects[id].properties).desired_rows(2));
                } else {
                    ui.add_enabled(false, egui::TextEdit::multiline(&mut blank_string).desired_rows(2));
                }

                ui.label("Rotation Axis:");
                let old_val = *rot_axis;
                ui.add_enabled_ui(selected_id.is_some(), |ui| {
                    ui.radio_value(rot_axis, SubsysMovementAxis::NONE, "None");
                    ui.radio_value(rot_axis, SubsysMovementAxis::XAXIS, "X-axis");
                    ui.radio_value(rot_axis, SubsysMovementAxis::YAXIS, "Y-axis");
                    ui.radio_value(rot_axis, SubsysMovementAxis::ZAXIS, "Z-axis");
                    ui.radio_value(rot_axis, SubsysMovementAxis::OTHER, "Other");
                });
                if old_val != *rot_axis {
                    self.model.sub_objects[selected_id.unwrap()].movement_axis = *rot_axis;
                }

                // DEBUG - prints total bsp node bbox volume at all depths (divided by actual top-level bbox volume so literal size doesn't matter)
                // Theoretically should be a decent metric for BSP tree efficiency; lower = better
                //
                // if ui.button("avg depth").clicked() {
                //     let node = &self.model.sub_objects[selected_id.unwrap()].bsp_data.collision_tree;
                //     let bbox_vol = match node {
                //         pof::BspNode::Split { bbox, .. } | pof::BspNode::Leaf { bbox, .. } => bbox.volume(),
                //     };
                //     let (sum_depth, size) = node.sum_depth_and_size();
                //     println!(
                //         "avg bbox depth = {}, size = {}, avg tree depth = {}",
                //         node.sum_of_bboxes() / bbox_vol,
                //         size,
                //         sum_depth as f32 / size as f32,
                //     );
                // }

                ui.separator();

                if let Some(id) = selected_id {
                    ui.label(RichText::new(format!("Vertices: {}", self.model.sub_objects[id].bsp_data.verts.len())).weak());
                    ui.label(RichText::new(format!("Normals: {}", self.model.sub_objects[id].bsp_data.norms.len())).weak());
                }
            }
            PropertiesPanel::Texture { texture_name } => {
                ui.horizontal(|ui| {
                    ui.heading("Textures");

                    ui.add_space(ui.available_width() - 70.0);

                    if ui.add_sized([70.0, ui.available_height()], egui::Button::new("🔃 Reload")).clicked() {
                        reload_textures = true;
                    }
                });
                ui.separator();

                let tex = if let TreeSelection::Textures(TextureSelection::Texture(tex)) = self.ui_state.tree_view_selection {
                    Some(&mut self.model.textures[tex.0 as usize])
                } else {
                    None
                };

                ui.label("Texture Name:");
                UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, tex, texture_name);
            }
            PropertiesPanel::Thruster {
                engine_subsys_string,
                position_string,
                normal_string,
                radius_string,
            } => {
                ui.heading("Thruster");
                ui.separator();

                let (bank_num, point_num) = match self.ui_state.tree_view_selection {
                    TreeSelection::Thrusters(ThrusterSelection::Bank(bank)) => (Some(bank), None),
                    TreeSelection::Thrusters(ThrusterSelection::BankPoint(bank, point)) => (Some(bank), Some(point)),
                    _ => (None, None),
                };

                let bank_idx_response = UiState::list_manipulator_widget(ui, bank_num, Some(self.model.thruster_banks.len()), "Bank");

                ui.add_space(10.0);

                ui.horizontal(|ui| {
                    ui.label("Engine Subsystem:");
                    if let Some(bank) = bank_num {
                        if self.warnings.contains(&Warning::ThrusterPropertiesInvalidVersion(bank)) {
                            UiState::set_widget_color(ui, Color32::YELLOW);
                        }
                        if ui.text_edit_singleline(engine_subsys_string).changed() {
                            pof::properties_update_field(&mut self.model.thruster_banks[bank].properties, "$engine_subsystem", engine_subsys_string);
                        }
                        UiState::reset_widget_color(ui);
                    } else {
                        ui.add_enabled(false, egui::TextEdit::multiline(&mut blank_string).desired_rows(1));
                    }
                });

                CollapsingHeader::new("Properties Raw").show(ui, |ui| {
                    if let Some(bank) = bank_num {
                        if self.warnings.contains(&Warning::ThrusterPropertiesInvalidVersion(bank)) {
                            UiState::set_widget_color(ui, Color32::YELLOW);
                        }
                        if ui
                            .add(egui::TextEdit::multiline(&mut self.model.thruster_banks[bank].properties).desired_rows(1))
                            .changed()
                        {
                            PofToolsGui::recheck_warnings(&mut self.warnings, &self.model, One(Warning::ThrusterPropertiesInvalidVersion(bank)));
                        }
                        UiState::reset_widget_color(ui);
                    } else {
                        ui.add_enabled(false, egui::TextEdit::multiline(&mut blank_string).desired_rows(1));
                    }
                });

                ui.separator();

                let point_idx_response =
                    UiState::list_manipulator_widget(ui, point_num, bank_num.map(|bank| self.model.thruster_banks[bank].glows.len()), "Point");

                ui.add_space(10.0);

                let (pos, norm, radius) =
                    if let TreeSelection::Thrusters(ThrusterSelection::BankPoint(bank, point)) = self.ui_state.tree_view_selection {
                        let ThrusterGlow { position, normal, radius } = &mut self.model.thruster_banks[bank as usize].glows[point];
                        (Some(position), Some(normal), Some(radius))
                    } else {
                        (None, None, None)
                    };

                ui.label("Radius:");
                UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, radius, radius_string);
                ui.label("Position:");
                UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, pos, position_string);
                ui.label("Normal:");
                UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, norm, normal_string);

                if let Some(response) = bank_idx_response {
                    let new_idx = response.apply(&mut self.model.thruster_banks);

                    self.ui_state.tree_view_selection = TreeSelection::Thrusters(ThrusterSelection::bank(new_idx));
                    properties_panel_dirty = true;
                    self.ui_state.viewport_3d_dirty = true;
                } else if let Some(response) = point_idx_response {
                    let new_idx = response.apply(&mut self.model.thruster_banks[bank_num.unwrap()].glows);

                    self.ui_state.tree_view_selection = TreeSelection::Thrusters(ThrusterSelection::bank_point(bank_num.unwrap(), new_idx));
                    properties_panel_dirty = true;
                    self.ui_state.viewport_3d_dirty = true;
                }
            }
            PropertiesPanel::Weapon { position_string, normal_string, offset_string } => {
                let (mut weapon_system, bank_num, point_num) = match self.ui_state.tree_view_selection {
                    TreeSelection::Weapons(WeaponSelection::Header) => {
                        ui.heading("Weapons");
                        (None, None, None)
                    }
                    TreeSelection::Weapons(WeaponSelection::PriHeader) => {
                        ui.heading("Primary Weapons");
                        (Some((&mut self.model.primary_weps, true)), None, None)
                    }
                    TreeSelection::Weapons(WeaponSelection::PriBank(bank)) => {
                        ui.heading("Primary Weapons");
                        (Some((&mut self.model.primary_weps, true)), Some(bank), None)
                    }
                    TreeSelection::Weapons(WeaponSelection::PriBankPoint(bank, point)) => {
                        ui.heading("Primary Weapons");
                        (Some((&mut self.model.primary_weps, true)), Some(bank), Some(point))
                    }
                    TreeSelection::Weapons(WeaponSelection::SecHeader) => {
                        ui.heading("Secondary Weapons");
                        (Some((&mut self.model.secondary_weps, false)), None, None)
                    }
                    TreeSelection::Weapons(WeaponSelection::SecBank(bank)) => {
                        ui.heading("Secondary Weapons");
                        (Some((&mut self.model.secondary_weps, false)), Some(bank), None)
                    }
                    TreeSelection::Weapons(WeaponSelection::SecBankPoint(bank, point)) => {
                        ui.heading("Secondary Weapons");
                        (Some((&mut self.model.secondary_weps, false)), Some(bank), Some(point))
                    }
                    _ => {
                        unreachable!();
                    }
                };
                let weapon_selection = if let TreeSelection::Weapons(selection) = self.ui_state.tree_view_selection {
                    selection
                } else {
                    unreachable!()
                };

                ui.separator();

                let bank_idx_response = UiState::list_manipulator_widget(ui, bank_num, weapon_system.as_ref().map(|weps| weps.0.len()), "Bank");

                ui.add_space(10.0);

                ui.separator();

                let point_idx_response = UiState::list_manipulator_widget(
                    ui,
                    point_num,
                    weapon_system.as_ref().and_then(|weps| bank_num.map(|bank| weps.0[bank].len())),
                    "Point",
                );

                ui.add_space(10.0);

                let (pos, norm, offset) =
                    if let TreeSelection::Weapons(WeaponSelection::PriBankPoint(bank, point) | WeaponSelection::SecBankPoint(bank, point)) =
                        self.ui_state.tree_view_selection
                    {
                        let WeaponHardpoint { position, normal, offset } = &mut weapon_system.as_mut().unwrap().0[bank as usize][point];
                        (Some(position), Some(normal), Some(offset))
                    } else {
                        (None, None, None)
                    };

                ui.label("Position:");
                UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, pos, position_string);
                ui.label("Normal:");
                UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, norm, normal_string);
                ui.label("Offset:");
                let offset_changed = UiState::model_value_edit(
                    &mut self.ui_state.viewport_3d_dirty,
                    ui,
                    self.warnings.contains(&Warning::WeaponOffsetInvalidVersion(weapon_selection)),
                    offset,
                    offset_string,
                )
                .changed();

                if let Some(response) = bank_idx_response {
                    let (weapon_system, is_primary) = weapon_system.unwrap();
                    let new_idx = response.apply(weapon_system);

                    self.ui_state.tree_view_selection = TreeSelection::Weapons(WeaponSelection::bank(is_primary, new_idx));
                    properties_panel_dirty = true;
                    self.ui_state.viewport_3d_dirty = true;
                } else if let Some(response) = point_idx_response {
                    let (weapon_system, is_primary) = weapon_system.unwrap();
                    let new_idx = response.apply(&mut weapon_system[bank_num.unwrap()]);

                    self.ui_state.tree_view_selection = TreeSelection::Weapons(WeaponSelection::bank_point(is_primary, bank_num.unwrap(), new_idx));
                    properties_panel_dirty = true;
                    self.ui_state.viewport_3d_dirty = true;
                }

                if offset_changed {
                    PofToolsGui::recheck_warnings(&mut self.warnings, &self.model, One(Warning::WeaponOffsetInvalidVersion(weapon_selection)));
                }
            }
            PropertiesPanel::DockingBay {
                name_string,
                position_string,
                fvec_string,
                uvec_ang,
                path_num,
            } => {
                ui.heading("Docking Bay");
                ui.separator();

                let bay_num = match self.ui_state.tree_view_selection {
                    TreeSelection::DockingBays(DockingSelection::Bay(bay)) => Some(bay),
                    _ => None,
                };

                let bay_idx_response = UiState::list_manipulator_widget(ui, bay_num, Some(self.model.docking_bays.len()), "Bay");

                ui.add_space(10.0);

                ui.horizontal(|ui| {
                    ui.label("Name:");
                    if let Some(bay) = bay_num {
                        if ui.text_edit_singleline(name_string).changed() {
                            pof::properties_update_field(&mut self.model.docking_bays[bay].properties, "$name", name_string);
                        }
                    } else {
                        ui.add_enabled(false, egui::TextEdit::singleline(&mut blank_string));
                    }
                });

                let mut subobj_names_list = vec![String::new()];
                subobj_names_list.extend(self.model.get_subobj_names().into_iter());

                let mut parent_id = if let Some(bay) = bay_num {
                    pof::properties_get_field(&self.model.docking_bays[bay].properties, "$parent_submodel").map_or(0, |parent_name| {
                        subobj_names_list
                            .iter()
                            .position(|name| name.to_lowercase() == parent_name.to_lowercase())
                            .unwrap_or(0)
                    })
                } else {
                    0 // doesnt matter
                };

                if let Some(new_subobj) = UiState::subobject_combo_box(ui, &subobj_names_list, &mut parent_id, bay_num, "Parent Object", None) {
                    pof::properties_update_field(
                        &mut self.model.docking_bays[bay_num.unwrap()].properties,
                        "$parent_submodel",
                        &subobj_names_list[new_subobj],
                    );
                    self.ui_state.viewport_3d_dirty = true;
                }

                // combo box list of path names
                //  no valid bay selected -> a single empty string
                //  valid bay, without a path -> list of paths, followed by a single empty string
                //  valid bay, with a path -> list of paths
                let paths = if let Some(bay) = bay_num {
                    let mut out: Vec<String> = self.model.paths.iter().map(|path| path.name.clone()).collect();
                    if self.model.docking_bays[bay].path.is_none() {
                        out.push(String::new());
                    }
                    out
                } else {
                    vec![String::new()]
                };
                // make da combo box
                ui.add_enabled_ui(matches!(bay_num, Some(_)), |ui| {
                    if egui::ComboBox::from_label("Path")
                        .show_index(ui, path_num, paths.len(), |i| paths[i].to_owned())
                        .changed()
                    {
                        // assign any changes
                        if *path_num == self.model.paths.len() {
                            self.model.docking_bays[bay_num.unwrap()].path = None
                        } else {
                            self.model.docking_bays[bay_num.unwrap()].path = Some(PathId(*path_num as u32))
                        }
                        PofToolsGui::recheck_warnings(&mut self.warnings, &self.model, All);
                    }
                });

                ui.separator();

                CollapsingHeader::new("Properties Raw").show(ui, |ui| {
                    if let Some(bay) = bay_num {
                        if ui
                            .add(egui::TextEdit::multiline(&mut self.model.docking_bays[bay].properties).desired_rows(1))
                            .changed()
                        {
                            if let Some(new_name) = pof::properties_get_field(&self.model.docking_bays[bay].properties, "$name") {
                                *name_string = new_name.to_string();
                            }
                        }
                    } else {
                        ui.add_enabled(false, egui::TextEdit::multiline(&mut String::new()).desired_rows(1));
                    }
                });

                ui.separator();

                ui.add_space(10.0);

                ui.label("Position:");
                let pos = bay_num.map(|num| &mut self.model.docking_bays[num].position);
                UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, pos, position_string);

                ui.label(RichText::new("Forward Vector:").color(Color32::from_rgb(140, 150, 210)));
                let norm = bay_num.map(|num| &mut self.model.docking_bays[num].fvec);
                if UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, norm, fvec_string).changed() {
                    let bay = &mut self.model.docking_bays[bay_num.unwrap()];
                    bay.uvec = Dock::orthonormalize(&bay.uvec.0.into(), &bay.fvec.0.into());
                }

                ui.label(RichText::new("Up Vector:").color(Color32::from_rgb(210, 140, 140)));
                ui.add_enabled_ui(bay_num.is_some(), |ui| {
                    if ui.add(DragValue::new(uvec_ang).speed(0.5)).changed() {
                        self.ui_state.viewport_3d_dirty = true;
                        self.model.docking_bays[bay_num.unwrap()].set_uvec_angle(uvec_ang.to_radians());
                        *uvec_ang %= 360.0;
                    }
                });

                // its annoyingly verbose to mix and match text styles/colors :/
                let mut job = LayoutJob::default();
                job.append("When ships dock, they will oppose their ", 0.0, TextFormat::default());
                job.append(
                    "forward vectors ",
                    0.0,
                    TextFormat {
                        color: Color32::from_rgb(140, 150, 210),
                        ..Default::default()
                    },
                );
                job.append("and match their ", 0.0, TextFormat::default());
                job.append(
                    "up vectors",
                    0.0,
                    TextFormat {
                        color: Color32::from_rgb(210, 140, 140),
                        ..Default::default()
                    },
                );
                job.append(".", 0.0, TextFormat::default());
                ui.label(job);

                if let Some(response) = bay_idx_response {
                    let new_idx = response.apply(&mut self.model.docking_bays);

                    self.ui_state.tree_view_selection = TreeSelection::DockingBays(DockingSelection::bay(new_idx));
                    properties_panel_dirty = true;
                    self.ui_state.viewport_3d_dirty = true;
                }
            }
            PropertiesPanel::GlowBank {
                disp_time_string,
                on_time_string,
                off_time_string,
                lod_string,
                glow_type_string,
                glow_texture_string,
                attached_subobj_idx,
                position_string,
                normal_string,
                radius_string,
            } => {
                ui.heading("Glow Bank");
                ui.separator();

                let (bank_num, point_num) = match self.ui_state.tree_view_selection {
                    TreeSelection::Glows(GlowSelection::Bank(bank)) => (Some(bank), None),
                    TreeSelection::Glows(GlowSelection::BankPoint(bank, point)) => (Some(bank), Some(point)),
                    _ => (None, None),
                };

                // no subobjects = no glow banks allowed
                let glow_banks_len_opt = (!self.model.sub_objects.is_empty()).then(|| self.model.glow_banks.len());
                let bank_idx_response = UiState::list_manipulator_widget(ui, bank_num, glow_banks_len_opt, "Bank");

                ui.add_space(10.0);

                ui.label("Glow Texture:");
                if let Some(bank) = bank_num {
                    if ui.add(egui::TextEdit::singleline(glow_texture_string).desired_rows(1)).changed() {
                        pof::properties_update_field(&mut self.model.glow_banks[bank].properties, "$glow_texture", glow_texture_string);
                    }
                } else {
                    ui.add_enabled(false, egui::TextEdit::singleline(&mut blank_string).desired_rows(1));
                }

                let subobj_names_list = self.model.get_subobj_names();

                if let Some(new_subobj) = UiState::subobject_combo_box(ui, &subobj_names_list, attached_subobj_idx, bank_num, "SubObject", None) {
                    self.model.glow_banks[bank_num.unwrap()].obj_parent = ObjectId(new_subobj as u32);
                }

                let (disp_time, on_time, off_time, lod, glow_type) =
                    if let TreeSelection::Glows(GlowSelection::BankPoint(bank, _)) = self.ui_state.tree_view_selection {
                        let GlowPointBank { disp_time, on_time, off_time, lod, glow_type, .. } = &mut self.model.glow_banks[bank as usize];

                        (Some(disp_time), Some(on_time), Some(off_time), Some(lod), Some(glow_type))
                    } else if let TreeSelection::Glows(GlowSelection::Bank(bank)) = self.ui_state.tree_view_selection {
                        let GlowPointBank { disp_time, on_time, off_time, lod, glow_type, .. } = &mut self.model.glow_banks[bank as usize];

                        (Some(disp_time), Some(on_time), Some(off_time), Some(lod), Some(glow_type))
                    } else {
                        (None, None, None, None, None)
                    };

                ui.horizontal(|ui| {
                    ui.label("LOD:");
                    UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, lod, lod_string);
                });

                ui.horizontal(|ui| {
                    ui.label("Type:");
                    UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, glow_type, glow_type_string);
                });

                ui.separator();

                if ui.checkbox(&mut self.glow_point_simulation, "Glow Point Simulation").clicked() {
                    self.glow_point_sim_start = std::time::Instant::now();
                    self.ui_state.viewport_3d_dirty = true; // for the case when this is disabled
                }

                ui.label("Displacement Time:");
                UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, disp_time, disp_time_string);

                ui.horizontal(|ui| {
                    ui.label("On Time:");
                    UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, on_time, on_time_string);
                });

                ui.horizontal(|ui| {
                    ui.label("Off Time:");
                    UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, off_time, off_time_string);
                });

                ui.separator();

                CollapsingHeader::new("Properties Raw").show(ui, |ui| {
                    if let Some(bank) = bank_num {
                        if ui
                            .add(egui::TextEdit::multiline(&mut self.model.glow_banks[bank].properties).desired_rows(1))
                            .changed()
                        {
                            println!("{}", &self.model.glow_banks[bank].properties);
                        }
                    } else {
                        ui.add_enabled(false, egui::TextEdit::multiline(&mut String::new()).desired_rows(1));
                    }
                });

                ui.separator();

                let glow_points = if let TreeSelection::Glows(GlowSelection::BankPoint(bank, _)) = self.ui_state.tree_view_selection {
                    Some(&mut self.model.glow_banks[bank as usize].glow_points)
                } else if let TreeSelection::Glows(GlowSelection::Bank(bank)) = self.ui_state.tree_view_selection {
                    Some(&mut self.model.glow_banks[bank as usize].glow_points)
                } else {
                    None
                };

                let point_idx_response = UiState::list_manipulator_widget(ui, point_num, glow_points.map(|list| list.len()), "Point");

                let (pos, norm, radius) = if let TreeSelection::Glows(GlowSelection::BankPoint(bank, point)) = self.ui_state.tree_view_selection {
                    let GlowPoint { position, normal, radius } = &mut self.model.glow_banks[bank].glow_points[point];

                    (Some(position), Some(normal), Some(radius))
                } else {
                    (None, None, None)
                };

                ui.add_space(10.0);

                ui.label("Radius:");
                UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, radius, radius_string);
                ui.label("Position:");
                UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, pos, position_string);
                ui.label("Normal:");
                UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, norm, normal_string);

                if let Some(response) = bank_idx_response {
                    let new_idx = response.apply(&mut self.model.glow_banks);

                    self.ui_state.tree_view_selection = TreeSelection::Glows(GlowSelection::bank(new_idx));
                    properties_panel_dirty = true;
                    self.ui_state.viewport_3d_dirty = true;
                } else if let Some(response) = point_idx_response {
                    let new_idx = response.apply(&mut self.model.glow_banks[bank_num.unwrap()].glow_points);

                    self.ui_state.tree_view_selection = TreeSelection::Glows(GlowSelection::bank_point(bank_num.unwrap(), new_idx));
                    properties_panel_dirty = true;
                    self.ui_state.viewport_3d_dirty = true;
                }
            }
            PropertiesPanel::SpecialPoint { radius_string, position_string, name_string } => {
                ui.heading("Special Point");
                ui.separator();

                let point_num = match self.ui_state.tree_view_selection {
                    TreeSelection::SpecialPoints(SpecialPointSelection::Point(point)) => Some(point),
                    _ => None,
                };

                let spec_point_idx_response = UiState::list_manipulator_widget(ui, point_num, Some(self.model.special_points.len()), "Point");

                ui.add_space(10.0);

                ui.horizontal(|ui| {
                    ui.label("Name:");
                    if let Some(point) = point_num {
                        ui.add(egui::TextEdit::singleline(&mut self.model.special_points[point].name));
                    } else {
                        ui.add_enabled(false, egui::TextEdit::singleline(name_string));
                    }
                });

                ui.separator();

                let types_display = vec!["", "Subsystem", "Shield point"];
                let types = vec!["", "subsystem", "shieldpoint"];
                let mut idx = 0;
                if let Some(point) = point_num {
                    if let Some(type_str) = pof::properties_get_field(&self.model.special_points[point].properties, "$special") {
                        if let Some(i) = types.iter().position(|str| *str == type_str) {
                            idx = i;
                        }
                    }
                }

                ui.add_enabled_ui(point_num.is_some(), |ui| {
                    if let Some(point) = point_num {
                        let mut changed = false;
                        egui::ComboBox::from_label("Type").selected_text(types_display[idx]).show_ui(ui, |ui| {
                            changed |= ui.selectable_value(&mut idx, 0, types_display[0]).changed();
                            changed |= ui.selectable_value(&mut idx, 1, types_display[1]).changed();
                            changed |= ui.selectable_value(&mut idx, 2, types_display[2]).changed();
                        });
                        if changed {
                            pof::properties_update_field(&mut self.model.special_points[point].properties, "$special", types[idx]);
                        }
                    } else {
                        egui::ComboBox::from_label("Type").show_ui(ui, |ui| {
                            ui.selectable_value(&mut idx, 0, "");
                        });
                    }
                });

                CollapsingHeader::new("Properties Raw").show(ui, |ui| {
                    if let Some(point) = point_num {
                        if ui
                            .add(egui::TextEdit::multiline(&mut self.model.special_points[point].properties).desired_rows(1))
                            .changed()
                        {
                            if let Some(new_name) = pof::properties_get_field(&self.model.special_points[point].properties, "$name") {
                                *name_string = new_name.to_string();
                            }
                        }
                    } else {
                        ui.add_enabled(false, egui::TextEdit::multiline(&mut String::new()).desired_rows(1));
                    }
                });

                ui.separator();

                ui.add_space(10.0);

                let (pos, radius) = if let TreeSelection::SpecialPoints(SpecialPointSelection::Point(point)) = self.ui_state.tree_view_selection {
                    let SpecialPoint { position, radius, .. } = &mut self.model.special_points[point];
                    (Some(position), Some(radius))
                } else {
                    (None, None)
                };
                ui.label("Radius:");
                UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, radius, radius_string);
                ui.label("Position:");
                UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, pos, position_string);

                if let Some(response) = spec_point_idx_response {
                    let new_idx = response.apply(&mut self.model.special_points);

                    self.ui_state.tree_view_selection = TreeSelection::SpecialPoints(SpecialPointSelection::point(new_idx));
                    properties_panel_dirty = true;
                    self.ui_state.viewport_3d_dirty = true;
                }
            }
            PropertiesPanel::Turret { position_string, normal_string, base_idx } => {
                ui.heading("Turret");
                ui.separator();

                let (turret_num, point_num) = match self.ui_state.tree_view_selection {
                    TreeSelection::Turrets(TurretSelection::Turret(turret)) => (Some(turret), None),
                    TreeSelection::Turrets(TurretSelection::TurretPoint(turret, point)) => (Some(turret), Some(point)),
                    _ => (None, None),
                };

                // no subobjects = no turrets allowed
                let turrets_len_opt = (!self.model.sub_objects.is_empty()).then(|| self.model.turrets.len());
                let turret_idx_response = UiState::list_manipulator_widget(ui, turret_num, turrets_len_opt, "Turret");

                ui.add_space(10.0);
                let subobj_names_list = self.model.get_subobj_names();

                if let Some(new_subobj) = UiState::subobject_combo_box(ui, &subobj_names_list, base_idx, turret_num, "Base object", None) {
                    self.model.turrets[turret_num.unwrap()].base_obj = ObjectId(new_subobj as u32);
                    PofToolsGui::recheck_errors(&mut self.errors, &self.model, One(Error::InvalidTurretGunSubobject(turret_num.unwrap())));
                }

                // turret gun subobjexct combo box is a bit trickier since we only want to show valid subobjects (and the currently used one,
                // which may be invalid)

                let mut gun_subobj_ids_list = vec![];
                let mut gun_subobj_idx = 0;
                let mut error_idx = None;
                // assemble the list of ids, and get the index of the currently being used one
                if let Some(num) = turret_num {
                    let (list, idx) = self
                        .model
                        .get_valid_gun_subobjects_for_turret(self.model.turrets[num].gun_obj, self.model.turrets[num].base_obj);
                    gun_subobj_ids_list = list;
                    gun_subobj_idx = idx;
                    if self.errors.contains(&Error::InvalidTurretGunSubobject(num)) {
                        for (i, &id) in gun_subobj_ids_list.iter().enumerate() {
                            if id == self.model.turrets[num].gun_obj {
                                error_idx = Some(i);
                            }
                        }
                    }
                }
                // assemble the string names list from the id list
                let gun_subobj_names_list = gun_subobj_ids_list
                    .iter()
                    .map(|id| self.model.sub_objects[*id].name.clone())
                    .collect::<Vec<_>>();

                // then make the combo box, giving it the list of names and the index
                if let Some(new_idx) =
                    UiState::subobject_combo_box(ui, &gun_subobj_names_list, &mut gun_subobj_idx, turret_num, "Gun object", error_idx)
                {
                    // the unwraps are ok here, if it were none, the combo box would be un-interactable
                    self.model.turrets[turret_num.unwrap()].gun_obj = gun_subobj_ids_list[new_idx];
                    PofToolsGui::recheck_errors(&mut self.errors, &self.model, One(Error::InvalidTurretGunSubobject(turret_num.unwrap())));
                    self.ui_state.viewport_3d_dirty = true;
                }

                let norm = if let TreeSelection::Turrets(TurretSelection::Turret(turret) | TurretSelection::TurretPoint(turret, _)) =
                    self.ui_state.tree_view_selection
                {
                    Some(&mut self.model.turrets[turret].normal)
                } else {
                    None
                };

                ui.label("Normal:");
                UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, norm, normal_string);

                ui.separator();
                ui.add(Label::new(RichText::new("Turret Fire Points").text_style(TextStyle::Button)));
                ui.separator();

                let point_idx_response =
                    UiState::list_manipulator_widget(ui, point_num, turret_num.map(|num| self.model.turrets[num].fire_points.len()), "Fire Point");

                ui.add_space(10.0);

                let pos = if let TreeSelection::Turrets(TurretSelection::TurretPoint(turret, point)) = self.ui_state.tree_view_selection {
                    Some(&mut self.model.turrets[turret as usize].fire_points[point])
                } else {
                    None
                };

                ui.label("Position:");
                UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, pos, position_string);

                if let Some(response) = turret_idx_response {
                    let new_idx = response.apply(&mut self.model.turrets);
                    PofToolsGui::recheck_errors(&mut self.errors, &self.model, All);

                    self.ui_state.tree_view_selection = TreeSelection::Turrets(TurretSelection::turret(new_idx));
                    properties_panel_dirty = true;
                    self.ui_state.viewport_3d_dirty = true;
                } else if let Some(response) = point_idx_response {
                    let new_idx = response.apply(&mut self.model.turrets[turret_num.unwrap()].fire_points);
                    PofToolsGui::recheck_errors(&mut self.errors, &self.model, All);

                    self.ui_state.tree_view_selection = TreeSelection::Turrets(TurretSelection::turret_point(turret_num.unwrap(), new_idx));
                    properties_panel_dirty = true;
                    self.ui_state.viewport_3d_dirty = true;
                }
            }
            PropertiesPanel::Path { name, parent_string, position_string, radius_string } => {
                ui.heading("Path");
                ui.separator();

                let (path_num, point_num) = match self.ui_state.tree_view_selection {
                    TreeSelection::Paths(PathSelection::Path(path)) => (Some(path), None),
                    TreeSelection::Paths(PathSelection::PathPoint(path, point)) => (Some(path), Some(point)),
                    _ => (None, None),
                };

                let path_idx_response = UiState::list_manipulator_widget(ui, path_num, Some(self.model.paths.len()), "Bank");

                ui.add_space(10.0);

                ui.label("Name:");
                if let Some(num) = path_num {
                    ui.add(egui::TextEdit::multiline(&mut self.model.paths[num].name).desired_rows(1));
                } else {
                    ui.add_enabled(false, egui::TextEdit::multiline(name).desired_rows(1));
                }

                ui.label("Parent:");
                if let Some(num) = path_num {
                    ui.add(egui::TextEdit::multiline(&mut self.model.paths[num].parent).desired_rows(1));
                } else {
                    ui.add_enabled(false, egui::TextEdit::multiline(parent_string).desired_rows(1));
                }

                ui.separator();

                let point_idx_response =
                    UiState::list_manipulator_widget(ui, point_num, path_num.map(|num| self.model.paths[num].points.len()), "Point");

                ui.add_space(10.0);

                let (radius, pos) = if let TreeSelection::Paths(PathSelection::PathPoint(path, point)) = self.ui_state.tree_view_selection {
                    let PathPoint { position, radius, .. } = &mut self.model.paths[path as usize].points[point];
                    (Some(radius), Some(position))
                } else {
                    (None, None)
                };

                ui.label("Radius:");
                UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, radius, radius_string);
                ui.label("Position:");
                UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, pos, position_string);

                if let Some(response) = path_idx_response {
                    if let IndexingButtonsResponse::Delete(idx) = response {
                        self.model.path_removal_fixup(PathId(idx as u32));
                        PofToolsGui::recheck_warnings(&mut self.warnings, &self.model, All);
                    }
                    let new_idx = response.apply(&mut self.model.paths);

                    self.ui_state.tree_view_selection = TreeSelection::Paths(PathSelection::path(new_idx));
                    properties_panel_dirty = true;
                    self.ui_state.viewport_3d_dirty = true;
                } else if let Some(response) = point_idx_response {
                    let new_idx = response.apply(&mut self.model.paths[path_num.unwrap()].points);

                    self.ui_state.tree_view_selection = TreeSelection::Paths(PathSelection::path_point(path_num.unwrap(), new_idx));
                    properties_panel_dirty = true;
                    self.ui_state.viewport_3d_dirty = true;
                }
            }
            PropertiesPanel::Shield => {
                ui.heading("Shield");
                ui.separator();
                if let Some(shield_data) = &self.model.shield_data {
                    ui.label(format!("{} vertices", shield_data.verts.len()));
                    ui.label(format!("{} polygons", shield_data.polygons.len()));
                } else {
                    ui.label("This model has no shield mesh.");
                }
            }
            PropertiesPanel::Insignia { lod_string, offset_string } => {
                ui.heading("Insignia");
                ui.separator();

                ui.add_space(10.0);

                let (lod, offset) = if let TreeSelection::Insignia(InsigniaSelection::Insignia(idx)) = self.ui_state.tree_view_selection {
                    let Insignia { detail_level, offset, .. } = &mut self.model.insignias[idx];
                    (Some(detail_level), Some(offset))
                } else {
                    (None, None)
                };

                ui.label("Detail Level:");
                UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, lod, lod_string);
                ui.label("Offset:");
                UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, offset, offset_string);
            }
            PropertiesPanel::EyePoint { position_string, normal_string, attached_subobj_idx } => {
                ui.heading("Eye Point");
                ui.separator();

                let eye_num = match self.ui_state.tree_view_selection {
                    TreeSelection::EyePoints(EyeSelection::EyePoint(point)) => Some(point),
                    _ => None,
                };

                // no subobjects = no eye points allowed
                let eye_points_len_opt = (!self.model.sub_objects.is_empty()).then(|| self.model.eye_points.len());
                let eye_idx_response = UiState::list_manipulator_widget(ui, eye_num, eye_points_len_opt, "Eye Point");

                ui.add_space(10.0);

                ui.add_enabled_ui(eye_num.is_some(), |ui| {
                    if let Some(num) = eye_num {
                        let name_list = self.model.get_subobj_names();
                        egui::ComboBox::from_label("Attached submodel")
                            .show_index(ui, attached_subobj_idx, self.model.sub_objects.len(), |i| name_list[i].to_owned());
                        self.model.eye_points[num].attached_subobj = ObjectId(*attached_subobj_idx as u32);
                    } else {
                        egui::ComboBox::from_label("Attached submodel").show_index(ui, attached_subobj_idx, 1, |_| format!(""));
                    }
                });

                ui.separator();

                ui.add_space(10.0);

                let (pos, norm) = if let TreeSelection::EyePoints(EyeSelection::EyePoint(point)) = self.ui_state.tree_view_selection {
                    let EyePoint { offset, normal, .. } = &mut self.model.eye_points[point];
                    (Some(offset), Some(normal))
                } else {
                    (None, None)
                };
                ui.label("Position:");
                UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, pos, position_string);
                ui.label("Normal:");
                UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, norm, normal_string);

                if let Some(response) = eye_idx_response {
                    let new_idx = response.apply(&mut self.model.eye_points);

                    self.ui_state.tree_view_selection = TreeSelection::EyePoints(EyeSelection::point(new_idx));
                    properties_panel_dirty = true;
                    self.ui_state.viewport_3d_dirty = true;
                }
            }
            PropertiesPanel::VisualCenter { position } => {
                ui.heading("Visual Center");
                ui.separator();

                ui.label("The visual center is treated as the center for things like the targeting box, or tech room.");

                if UiState::parsable_text_edit(ui, &mut self.model.visual_center, position) {
                    self.viewport_3d_dirty = true;
                }
            }
            PropertiesPanel::Comments => {
                ui.heading("Comments");
                ui.separator();
                ui.text_edit_multiline(&mut self.model.comments);
            }
        }

        if reload_textures {
            self.load_textures();
        }

        if properties_panel_dirty {
            self.ui_state.refresh_properties_panel(&self.model);
        }
    }
}
