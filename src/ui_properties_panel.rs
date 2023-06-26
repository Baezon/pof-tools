#![allow(clippy::unnecessary_lazy_evaluations)]
use std::collections::HashMap;
use std::str::FromStr;

use egui::{style::Widgets, text::LayoutJob, CollapsingHeader, Color32, DragValue, Label, Response, RichText, TextEdit, TextFormat, TextStyle, Ui};
use glium::Display;
use itertools::Itertools;
use nalgebra_glm::TMat4;
use pof::{
    Dock, Error, EyePoint, GlowPoint, GlowPointBank, Insignia, ObjectId, PathId, PathPoint, Set::*, SpecialPoint, SubsysRotationAxis,
    SubsysRotationType, SubsysTranslationAxis, SubsysTranslationType, ThrusterGlow, Vec3d, Warning, WeaponHardpoint,
};

use crate::Model;

use crate::ui::{
    DockingTreeValue, EyeTreeValue, GlowTreeValue, IndexingButtonsAction, InsigniaTreeValue, PathTreeValue, PofToolsGui, SpecialPointTreeValue,
    SubObjectTreeValue, TextureTreeValue, ThrusterTreeValue, TreeValue, TurretTreeValue, UiState, UndoAction, WeaponTreeValue, ERROR_RED, LIGHT_BLUE,
    LIGHT_ORANGE, WARNING_YELLOW,
};

const NON_BREAK_SPACE: char = '\u{00A0}';

pub enum IndexingButtonsResponse<T: Clone> {
    Switch(usize),
    Copy(usize),
    Delete(usize),
    Push,
    Insert(usize, Box<T>),
}
impl<T: Clone> IndexingButtonsResponse<T> {
    /// applies the response, manipulating the data vector, returning the new index the UI should switch to (if any)
    /// and mutates itself into its inverse, for the benefit of the undo system
    pub fn apply(&mut self, data_vec: &mut Vec<T>) -> Option<usize>
    where
        T: Default,
    {
        match *self {
            IndexingButtonsResponse::Switch(idx) => {
                assert!(idx < data_vec.len());
                Some(idx)
            }
            IndexingButtonsResponse::Copy(idx) => {
                assert!(idx < data_vec.len());
                let new_idx = data_vec.len();
                let item = data_vec[idx].clone();
                data_vec.push(item);
                *self = IndexingButtonsResponse::Delete(data_vec.len() - 1);
                Some(new_idx)
            }
            IndexingButtonsResponse::Delete(idx) => {
                assert!(idx < data_vec.len());
                let data = data_vec.remove(idx);
                *self = IndexingButtonsResponse::Insert(idx, Box::new(data));
                if idx < data_vec.len() {
                    Some(idx)
                } else {
                    idx.checked_sub(1)
                }
            }
            IndexingButtonsResponse::Push => {
                let new_idx = data_vec.len();
                data_vec.push(Default::default());
                *self = IndexingButtonsResponse::Delete(data_vec.len() - 1);
                Some(new_idx)
            }
            IndexingButtonsResponse::Insert(idx, _) => {
                // swap the Insert with a Delete, and cleanly extract the data from the box into the vector
                if let IndexingButtonsResponse::Insert(idx, data) = std::mem::replace(self, IndexingButtonsResponse::Delete(idx)) {
                    data_vec.insert(idx, *data);
                } else {
                    unreachable!();
                }
                None
            }
        }
    }

    pub fn get_new_ui_idx(&self, data_vec: &Vec<T>) -> Option<usize>
    where
        T: Default,
    {
        match *self {
            IndexingButtonsResponse::Switch(idx) => {
                assert!(idx < data_vec.len());
                Some(idx)
            }
            IndexingButtonsResponse::Copy(idx) => {
                assert!(idx < data_vec.len());
                Some(data_vec.len())
            }
            IndexingButtonsResponse::Delete(idx) => {
                assert!(idx < data_vec.len());
                if idx < data_vec.len() - 1 {
                    Some(idx)
                } else {
                    idx.checked_sub(1)
                }
            }
            IndexingButtonsResponse::Push => Some(data_vec.len()),
            IndexingButtonsResponse::Insert(..) => None,
        }
    }
}

impl UiState {
    fn set_widget_color(ui: &mut Ui, color: Color32) {
        ui.visuals_mut().widgets.hovered.fg_stroke.color = color;
        ui.visuals_mut().widgets.inactive.fg_stroke.color = color;
        ui.visuals_mut().widgets.noninteractive.fg_stroke.color = color;
        ui.visuals_mut().widgets.active.fg_stroke.color = color;
        ui.visuals_mut().widgets.open.fg_stroke.color = color;
    }

    fn reset_widget_color(ui: &mut Ui) {
        ui.visuals_mut().widgets = Widgets::default();
    }

    #[must_use]
    fn list_manipulator_widget<T: Clone>(
        ui: &mut Ui, current_num: Option<usize>, list_len: Option<usize>, index_name: &str,
    ) -> Option<IndexingButtonsResponse<T>> {
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
            ui.visuals_mut().override_text_color = Some(ERROR_RED);
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
        active_warning_idx: Option<usize>,
    ) -> Option<usize> {
        let mut ret = None;

        ui.add_enabled_ui(selector_value.is_some(), |ui| {
            if selector_value.is_some() {
                let color = if active_error_idx.is_some() {
                    UiState::set_widget_color(ui, ERROR_RED);
                    ERROR_RED
                } else if active_warning_idx.is_some() {
                    UiState::set_widget_color(ui, WARNING_YELLOW);
                    WARNING_YELLOW
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
                ui.visuals_mut().override_text_color = Some(ERROR_RED);
            } else if active_warning {
                ui.visuals_mut().override_text_color = Some(WARNING_YELLOW);
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
                        ui.selectable_value(&mut transform_window.vector, format!("1, 0, 0"), "X-axis");
                        ui.separator();
                        ui.selectable_value(&mut transform_window.vector, format!("0, 1, 0"), "Y-axis");
                        ui.separator();
                        ui.selectable_value(&mut transform_window.vector, format!("0, 0, 1"), "Z-axis");
                        ui.separator();
                    });
                    ui.separator();
                    ui.label("Axis:");

                    if transform_window.vector.parse::<Vec3d>().is_err() {
                        ui.visuals_mut().override_text_color = Some(ERROR_RED);
                    }
                    ui.text_edit_singleline(&mut transform_window.vector);
                    ui.visuals_mut().override_text_color = None;

                    ui.label("Angle:");
                    if transform_window.value.parse::<f32>().is_err() {
                        ui.visuals_mut().override_text_color = Some(ERROR_RED);
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
                    let parsed_val = transform_window.value.parse::<f32>();
                    if parsed_val.map_or(true, |val| val.abs() < 1e-6) {
                        ui.visuals_mut().override_text_color = Some(ERROR_RED);
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
                        valid_input = scalar.abs() >= 1e-6;
                    }
                }
                TransformType::Translate => {
                    ui.label("Translation Vector:");

                    if transform_window.vector.parse::<Vec3d>().is_err() {
                        ui.visuals_mut().override_text_color = Some(ERROR_RED);
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
            TreeValue::Header => {
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
            TreeValue::SubObjects(subobj_tree_select) => match subobj_tree_select {
                SubObjectTreeValue::Header => self.properties_panel = PropertiesPanel::default_subobject(),
                SubObjectTreeValue::SubObject(id) => {
                    self.properties_panel = PropertiesPanel::SubObject {
                        bbox_max_string: format!("{}", model.sub_objects[id].bbox.max),
                        bbox_min_string: format!("{}", model.sub_objects[id].bbox.min),
                        offset_string: format!("{}", model.sub_objects[id].offset),
                        radius_string: format!("{}", model.sub_objects[id].radius),
                        is_debris_check: model.sub_objects[id].is_debris_model,
                        name: format!("{}", model.sub_objects[id].name),
                        rot_axis: model.sub_objects[id].rotation_axis,
                        trans_axis: model.sub_objects[id].translation_axis,
                        transform_window: Default::default(),
                    }
                }
            },
            TreeValue::Textures(tex_tree_select) => match tex_tree_select {
                TextureTreeValue::Header => self.properties_panel = PropertiesPanel::default_texture(),
                TextureTreeValue::Texture(texture_id) => {
                    self.properties_panel = PropertiesPanel::Texture {
                        texture_name: format!("{}", model.textures[texture_id.0 as usize]),
                    }
                }
            },
            TreeValue::Thrusters(thruster_tree_select) => match thruster_tree_select {
                ThrusterTreeValue::Header => self.properties_panel = PropertiesPanel::default_thruster(),
                ThrusterTreeValue::Bank(bank) => {
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
                ThrusterTreeValue::BankPoint(bank, point) => {
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
            TreeValue::Weapons(weapons_tree_select) => match weapons_tree_select {
                WeaponTreeValue::PriBankPoint(bank_idx, point_idx) => {
                    self.properties_panel = PropertiesPanel::Weapon {
                        position_string: format!("{}", model.primary_weps[bank_idx][point_idx].position),
                        normal_string: format!("{}", model.primary_weps[bank_idx][point_idx].normal.0),
                        offset_string: format!("{}", model.primary_weps[bank_idx][point_idx].offset),
                    }
                }
                WeaponTreeValue::SecBankPoint(bank_idx, point_idx) => {
                    self.properties_panel = PropertiesPanel::Weapon {
                        position_string: format!("{}", model.secondary_weps[bank_idx][point_idx].position),
                        normal_string: format!("{}", model.secondary_weps[bank_idx][point_idx].normal.0),
                        offset_string: format!("{}", model.secondary_weps[bank_idx][point_idx].offset),
                    }
                }
                _ => self.properties_panel = PropertiesPanel::default_weapon(),
            },
            TreeValue::DockingBays(docking_select) => match docking_select {
                DockingTreeValue::Bay(bay) => {
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
            TreeValue::Glows(glow_select) => match glow_select {
                GlowTreeValue::BankPoint(bank, point) => {
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
                GlowTreeValue::Bank(bank) => {
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
            TreeValue::SpecialPoints(special_select) => match special_select {
                SpecialPointTreeValue::Point(point) => {
                    self.properties_panel = PropertiesPanel::SpecialPoint {
                        name_string: format!("{}", model.special_points[point].name),
                        position_string: format!("{}", model.special_points[point].position),
                        radius_string: format!("{}", model.special_points[point].radius),
                    }
                }
                _ => self.properties_panel = PropertiesPanel::default_special_point(),
            },
            TreeValue::Turrets(turret_selection) => match turret_selection {
                TurretTreeValue::TurretPoint(turret, point) => {
                    self.properties_panel = PropertiesPanel::Turret {
                        normal_string: format!("{}", model.turrets[turret].normal.0),
                        base_idx: model.turrets[turret].base_obj.0 as usize,
                        position_string: format!("{}", model.turrets[turret].fire_points[point]),
                    }
                }
                TurretTreeValue::Turret(turret) => {
                    self.properties_panel = PropertiesPanel::Turret {
                        normal_string: format!("{}", model.turrets[turret].normal.0),
                        base_idx: model.turrets[turret].base_obj.0 as usize,
                        position_string: Default::default(),
                    }
                }
                _ => self.properties_panel = PropertiesPanel::default_turret(),
            },
            TreeValue::Paths(path_selection) => match path_selection {
                PathTreeValue::PathPoint(path, point) => {
                    self.properties_panel = PropertiesPanel::Path {
                        name: format!("{}", model.paths[path].name),
                        parent_string: format!("{}", model.paths[path].parent),
                        position_string: format!("{}", model.paths[path].points[point].position),
                        radius_string: format!("{}", model.paths[path].points[point].radius),
                    }
                }
                PathTreeValue::Path(path) => {
                    self.properties_panel = PropertiesPanel::Path {
                        name: format!("{}", model.paths[path].name),
                        parent_string: format!("{}", model.paths[path].parent),
                        position_string: Default::default(),
                        radius_string: Default::default(),
                    }
                }
                _ => self.properties_panel = PropertiesPanel::default_path(),
            },
            TreeValue::Insignia(insig_selection) => match insig_selection {
                InsigniaTreeValue::Insignia(idx) => {
                    self.properties_panel = PropertiesPanel::Insignia {
                        lod_string: format!("{}", model.insignias[idx].detail_level),
                        offset_string: format!("{}", model.insignias[idx].offset),
                    }
                }
                _ => self.properties_panel = PropertiesPanel::default_insignia(),
            },
            TreeValue::EyePoints(eye_selection) => match eye_selection {
                EyeTreeValue::EyePoint(idx) => {
                    self.properties_panel = PropertiesPanel::EyePoint {
                        position_string: format!("{}", model.eye_points[idx].position),
                        normal_string: format!("{}", model.eye_points[idx].normal.0),
                        attached_subobj_idx: model.eye_points[idx].attached_subobj.map_or(model.sub_objects.len(), |id| id.0 as usize),
                    }
                }
                _ => self.properties_panel = PropertiesPanel::default_eye(),
            },
            TreeValue::Shield => self.properties_panel = PropertiesPanel::Shield, // nothing mutable to refresh! woohoo!'
            TreeValue::VisualCenter => self.properties_panel = PropertiesPanel::VisualCenter { position: format!("{}", model.visual_center) },
            TreeValue::Comments => self.properties_panel = PropertiesPanel::Comments,
        }
    }
}

#[derive(Default)]
pub struct TransformWindow {
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

pub enum PropertiesPanel {
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
        rot_axis: SubsysRotationAxis,
        trans_axis: SubsysTranslationAxis,
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
            rot_axis: SubsysRotationAxis::None,
            trans_axis: SubsysTranslationAxis::None,
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
    pub(crate) fn do_properties_panel(
        &mut self, ui: &mut egui::Ui, ctx: &egui::Context, display: &Display, undo_history: &mut undo::History<UndoAction>,
    ) {
        let mut reload_textures = false;
        let mut buffer_ids_to_rebuild = vec![];
        let mut rebuild_all_buffers = false;
        let mut merge_duplicate_textures = false;

        macro_rules! select_new_tree_val {
            ($x:expr) => {
                self.ui_state.select_new_tree_val($x);
                self.ui_state.properties_panel_dirty = true;
            };
        }

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
                        self.ui_state.properties_panel_dirty = true;
                        bbox_changed = true;
                    }
                    display_bbox = response.hovered() || response.has_focus() || display_bbox;
                });

                ui.horizontal(|ui| {
                    ui.label("Min:");
                    let response = UiState::model_value_edit(
                        &mut self.ui_state.viewport_3d_dirty,
                        ui,
                        self.model.warnings.contains(&Warning::BBoxTooSmall(None)),
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
                        self.model.warnings.contains(&Warning::BBoxTooSmall(None)),
                        Some(&mut self.model.header.bbox.max),
                        bbox_max_string,
                    );

                    if response.changed() {
                        bbox_changed = true;
                    }
                    self.ui_state.display_bbox = response.hovered() || response.has_focus() || display_bbox;
                });

                let bbox = &self.model.header.bbox;
                ui.horizontal_wrapped(|ui| {
                    ui.label(format!("Width:{NON_BREAK_SPACE}{:.1}", bbox.x_width()));
                    ui.label(format!("Height:{NON_BREAK_SPACE}{:.1}", bbox.y_height()));
                    ui.label(format!("Length:{NON_BREAK_SPACE}{:.1}", bbox.z_length()));
                });

                ui.separator();

                let mut radius_changed = false;
                let mut display_radius = false;
                ui.horizontal(|ui| {
                    ui.add(egui::Label::new("Radius:"));
                    let response = ui.button("Recalculate");

                    if response.clicked() {
                        self.model.recalc_radius();
                        radius_changed = true;
                        self.ui_state.properties_panel_dirty = true;
                    }
                    display_radius = response.hovered() || response.has_focus() || display_radius;
                });

                let response = UiState::model_value_edit(
                    &mut self.ui_state.viewport_3d_dirty,
                    ui,
                    self.model.warnings.contains(&Warning::RadiusTooSmall(None)),
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
                        self.ui_state.properties_panel_dirty = true;
                    }
                });
                UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, Some(&mut self.model.header.mass), mass_string);

                ui.horizontal(|ui| {
                    ui.add(egui::Label::new("Moment of Inertia:"));
                    if ui.button("Recalculate").clicked() {
                        self.model.recalc_moi();
                        self.ui_state.properties_panel_dirty = true;
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
                    self.model.recheck_warnings(One(Warning::RadiusTooSmall(None)));
                }
                if bbox_changed {
                    self.model.recheck_warnings(One(Warning::BBoxTooSmall(None)));
                    self.model.recheck_warnings(One(Warning::InvertedBBox(None)));
                }

                ui.separator();

                ui.label("Detail Levels:");

                // save any changed detail levels, we cant modify the list while displaying it, of course
                let mut changed_detail = None;

                for (i, &id) in self.model.header.detail_levels.iter().enumerate() {
                    let mut combo_idx = 0;
                    let mut listed_objects = vec![];
                    let mut active_warning_idx = None;

                    // add all the valid objects
                    for subobj in &self.model.sub_objects {
                        if subobj.parent().is_some() || subobj.is_debris_model {
                            continue;
                        }

                        if subobj.obj_id == id {
                            combo_idx = listed_objects.len();
                        }

                        listed_objects.push(subobj.name.clone());
                    }

                    // if we have an invalid one add that too
                    if self.model.sub_objects[id].parent().is_some() {
                        combo_idx = listed_objects.len();
                        active_warning_idx = Some(combo_idx);
                        listed_objects.push(self.model.sub_objects[id].name.clone());
                    }

                    if self.model.warnings.contains(&Warning::DuplicateDetailLevel(id)) {
                        active_warning_idx = Some(combo_idx);
                    }

                    //finally add a "None" option
                    listed_objects.push(format!("None"));

                    if let Some(new_idx) =
                        UiState::subobject_combo_box(ui, &listed_objects, &mut combo_idx, Some(()), &format!("- {}", i), None, active_warning_idx)
                    {
                        if new_idx == listed_objects.len() - 1 {
                            changed_detail = Some((i, None));
                        } else {
                            changed_detail = Some((i, self.model.get_obj_id_by_name(&listed_objects[new_idx])));
                        }
                    }
                }

                // add a special dummy detail level so that users can add new ones
                {
                    let mut listed_objects: Vec<String> = self
                        .model
                        .sub_objects
                        .iter()
                        .filter(|subobj| subobj.parent().is_none() && !subobj.is_debris_model)
                        .map(|subobj| subobj.name.clone())
                        .collect();
                    listed_objects.push(format!("None"));

                    let mut combo_idx = listed_objects.len() - 1;
                    if let Some(new_idx) = UiState::subobject_combo_box(
                        ui,
                        &listed_objects,
                        &mut combo_idx,
                        Some(()),
                        &format!("- {}", self.model.header.detail_levels.len()),
                        None,
                        None,
                    ) {
                        if new_idx == listed_objects.len() - 1 {
                            changed_detail = Some((self.model.header.detail_levels.len(), None));
                        } else {
                            changed_detail = Some((self.model.header.detail_levels.len(), self.model.get_obj_id_by_name(&listed_objects[new_idx])));
                        }
                    }
                }

                //now we'll handle any changes
                if let Some((level, id_opt)) = changed_detail {
                    if let Some(new_id) = id_opt {
                        // change to a new idx
                        if level == self.model.header.detail_levels.len() {
                            // add a new detail level
                            self.model.header.detail_levels.push(new_id);
                        } else if let Some(swapped_level) = self.model.header.detail_levels.iter().position(|&id| id == new_id) {
                            // swap with an existing level
                            let swapped_id = self.model.header.detail_levels[level];
                            self.model.header.detail_levels[level] = new_id;
                            self.model.header.detail_levels[swapped_level] = swapped_id;
                        } else {
                            self.model.header.detail_levels[level] = new_id;
                        }
                    } else {
                        // Remove a detail level
                        // No holes allowed, so truncate
                        self.model.header.detail_levels.truncate(level);
                    }

                    self.model.recheck_warnings(All);
                    // FIX
                }

                ui.separator();

                if ui.add(egui::Button::new("Transform Mesh")).clicked() {
                    transform_window.open = true;
                }
                if let Some(matrix) = UiState::show_transform_window(ctx, transform_window) {
                    self.model.apply_transform(&matrix);
                    rebuild_all_buffers = true;
                    self.ui_state.viewport_3d_dirty = true;
                    self.ui_state.properties_panel_dirty = true;
                }

                ui.add_space(10.0);

                let mut num_verts = 0;
                let mut num_norms = 0;
                for subobj in &self.model.sub_objects {
                    num_verts += subobj.bsp_data.verts.len();
                    num_norms += subobj.bsp_data.norms.len();
                }
                ui.label(RichText::new(format!("Total vertices: {}", num_verts)).weak());
                ui.label(RichText::new(format!("Total normals: {}", num_norms)).weak());
            }
            PropertiesPanel::SubObject {
                bbox_min_string,
                bbox_max_string,
                name,
                offset_string,
                radius_string,
                is_debris_check,
                rot_axis,
                trans_axis,
                transform_window,
            } => {
                ui.heading("SubObject");
                ui.separator();

                let selected_id = if let TreeValue::SubObjects(SubObjectTreeValue::SubObject(id)) = self.ui_state.tree_view_selection {
                    Some(id)
                } else {
                    None
                };

                // Name edit ================================================================

                if let Some(id) = selected_id {
                    let mut text = RichText::new("Name:");
                    if self
                        .model
                        .errors
                        .contains(&Error::DuplicateSubobjectName(self.model.sub_objects[id].name.clone()))
                        || self.model.errors.contains(&Error::UnnamedSubObject(id))
                    {
                        text = text.color(ERROR_RED);
                    }
                    ui.label(text);
                    let old_name = self.model.sub_objects[id].name.clone();
                    if ui.add(egui::TextEdit::singleline(&mut self.model.sub_objects[id].name)).changed() {
                        self.model.recheck_warnings(One(Warning::SubObjectNameTooLong(id)));
                        self.model.recheck_errors(One(Error::UnnamedSubObject(id)));
                        self.model.recheck_errors(One(Error::DuplicateSubobjectName(old_name)));
                        self.model
                            .pof_model
                            .recheck_errors(One(Error::DuplicateSubobjectName(self.model.pof_model.sub_objects[id].name.clone())));
                        self.model.recalc_semantic_name_links();
                    }
                } else {
                    ui.label("Name:");
                    ui.add_enabled(false, egui::TextEdit::singleline(name));
                }

                ui.add_space(5.0);

                // Is Debris Object Checkbox ================================================================

                let num_debris = self.model.num_debris_objects();
                let cannot_be_debris =
                    num_debris >= pof::MAX_DEBRIS_OBJECTS || selected_id.map_or(false, |id| self.model.header.detail_levels.contains(&id));

                ui.add_enabled_ui(selected_id.map_or(false, |id| !cannot_be_debris || self.model.sub_objects[id].is_debris_model), |ui| {
                    if selected_id.map_or(false, |id| {
                        self.model.sub_objects[id].is_debris_model
                            && (self.model.header.detail_levels.contains(&id) || num_debris > pof::MAX_DEBRIS_OBJECTS)
                    }) {
                        UiState::set_widget_color(ui, ERROR_RED);
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
                        self.model.recheck_errors(One(Error::TooManyDebrisObjects));
                        self.model.recheck_errors(One(Error::DetailAndDebrisObj(selected_id.unwrap())));
                    }

                    UiState::reset_widget_color(ui);
                });

                ui.add_space(5.0);

                // Bounding Box edit ================================================================

                let mut bbox_changed = false;
                let mut display_bbox = false;
                ui.horizontal(|ui| {
                    ui.label("Bounding Box:");
                    let response = ui.add_enabled(selected_id.is_some(), egui::Button::new("Recalculate"));

                    if response.clicked() {
                        self.model.sub_objects[selected_id.unwrap()].recalc_bbox();
                        self.ui_state.properties_panel_dirty = true;
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
                    self.model.recheck_warnings(One(Warning::BBoxTooSmall(selected_id)));
                    self.model.recheck_warnings(One(Warning::InvertedBBox(selected_id)));
                }

                if let Some(id) = selected_id {
                    let bbox = &self.model.sub_objects[id].bbox;
                    ui.horizontal_wrapped(|ui| {
                        ui.label(format!("Width:{NON_BREAK_SPACE}{:.1}", bbox.x_width()));
                        ui.label(format!("Height:{NON_BREAK_SPACE}{:.1}", bbox.y_height()));
                        ui.label(format!("Length:{NON_BREAK_SPACE}{:.1}", bbox.z_length()));
                    });
                }

                ui.separator();

                // Offset edit ================================================================

                ui.horizontal_wrapped(|ui| {
                    ui.label("Offset:");
                    let response = ui.add_enabled(selected_id.is_some(), egui::Button::new("Recalculate"));

                    if response.clicked() {
                        self.model.recalc_subobj_offset(selected_id.unwrap());
                        self.model.recheck_warnings(One(Warning::RadiusTooSmall(selected_id)));

                        self.ui_state.viewport_3d_dirty = true;
                        buffer_ids_to_rebuild.push(selected_id.unwrap());
                        self.ui_state.properties_panel_dirty = true;
                    }
                    self.ui_state.display_origin |= response.hovered() || response.has_focus();
                });

                if let Some(id) = selected_id {
                    if offset_string.parse::<Vec3d>().is_err() {
                        ui.visuals_mut().override_text_color = Some(ERROR_RED);
                    }
                    let response = ui.text_edit_singleline(offset_string);
                    if response.changed() {
                        if let Ok(parsed_string) = offset_string.parse() {
                            if self.ui_state.move_only_offset {
                                self.model.subobj_move_only_offset(id, parsed_string);
                                buffer_ids_to_rebuild.push(selected_id.unwrap());
                            } else {
                                self.model.sub_objects[id].offset = parsed_string;
                            }
                            self.ui_state.viewport_3d_dirty = true;
                        }
                    }
                    self.ui_state.display_origin |= response.hovered() || response.has_focus();
                    ui.visuals_mut().override_text_color = None;
                } else {
                    ui.add_enabled(false, TextEdit::singleline(offset_string));
                }

                let response = ui
                    .add_enabled(selected_id.is_some(), egui::Checkbox::new(&mut self.ui_state.move_only_offset, "Modify Offset Only"))
                    .on_hover_text(
                        "Changes will affect only the offset/center of this subobject, all other geometry remains in place.\nThe radius will be recalculated.",
                    );

                self.ui_state.display_origin |= response.hovered() || response.has_focus();

                ui.add_space(5.0);

                // Radius edit ================================================================

                let mut radius_changed = false;
                let mut display_radius = false;
                ui.horizontal(|ui| {
                    ui.label("Radius:");
                    let response = ui.add_enabled(selected_id.is_some(), egui::Button::new("Recalculate"));

                    if response.clicked() {
                        self.model.sub_objects[selected_id.unwrap()].recalc_radius();
                        self.ui_state.properties_panel_dirty = true;
                        radius_changed = true;
                    }
                    display_radius = response.hovered() || response.has_focus() || display_radius;
                });

                let response = UiState::model_value_edit(
                    &mut self.ui_state.viewport_3d_dirty,
                    ui,
                    self.model.warnings.contains(&Warning::RadiusTooSmall(selected_id)),
                    selected_id.map(|id| &mut self.model.sub_objects[id].radius),
                    radius_string,
                );

                if response.changed() {
                    radius_changed = true;
                }
                self.ui_state.display_radius = response.hovered() || response.has_focus() || display_radius;

                if radius_changed {
                    self.model.recheck_warnings(One(Warning::RadiusTooSmall(selected_id)));
                }

                ui.separator();

                // Transform Mesh button ================================================================

                if ui
                    .add_enabled(matches!(selected_id, Some(_)), egui::Button::new("Transform Mesh"))
                    .clicked()
                {
                    transform_window.open = true;
                }
                if let Some(matrix) = UiState::show_transform_window(ctx, transform_window) {
                    if let Some(id) = selected_id {
                        self.model.apply_subobj_transform(id, &matrix, false);
                        self.ui_state.viewport_3d_dirty = true;
                        self.ui_state.properties_panel_dirty = true;

                        self.model
                            .do_for_recursive_subobj_children(id, &mut |subobj| buffer_ids_to_rebuild.push(subobj.obj_id));
                    }
                }

                // Parent subobject combo box ================================================================

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

                if let Some(new_parent) = UiState::subobject_combo_box(ui, &subobj_names_list, &mut combo_idx, selected_id, "Parent", None, None) {
                    self.model.make_orphan(selected_id.unwrap());

                    if new_parent != 0 {
                        let parent_id = self.model.get_obj_id_by_name(&subobj_names_list[new_parent]).unwrap();
                        self.model.make_parent(parent_id, selected_id.unwrap());
                    }

                    //Error::InvalidTurretGunSubobject(())
                    //Error::DetailObjWithParent(())
                    self.model.recheck_errors(All);
                }

                // Properties edit ================================================================

                ui.label("Properties:");
                if let Some(id) = selected_id {
                    if self.model.sub_objects[id].uvec_fvec().is_some() {
                        self.ui_state.display_uvec_fvec = true;
                    }
                    if ui
                        .add(egui::TextEdit::multiline(&mut self.model.sub_objects[id].properties).desired_rows(2))
                        .changed()
                    {
                        self.model.recheck_warnings(One(Warning::SubObjectPropertiesTooLong(id)));
                        self.ui_state.viewport_3d_dirty = true; // There may be changes to the uvec/fvec
                    };
                } else {
                    ui.add_enabled(false, egui::TextEdit::multiline(&mut blank_string).desired_rows(2));
                }

                // Rot axis radio buttons ================================================================

                ui.horizontal(|ui| {
                    ui.vertical(|ui| {
                        ui.label("Rotation Axis:");
                        let old_val = *rot_axis;
                        ui.add_enabled_ui(selected_id.is_some(), |ui| {
                            ui.radio_value(rot_axis, SubsysRotationAxis::None, "None");
                            ui.radio_value(rot_axis, SubsysRotationAxis::X, "X-axis");
                            ui.radio_value(rot_axis, SubsysRotationAxis::Y, "Y-axis");
                            ui.radio_value(rot_axis, SubsysRotationAxis::Z, "Z-axis");
                            ui.radio_value(rot_axis, SubsysRotationAxis::Other, "Other");
                        });
                        if old_val != *rot_axis {
                            let obj = &mut self.model.sub_objects[selected_id.unwrap()];
                            obj.rotation_axis = *rot_axis;
                            if *rot_axis == SubsysRotationAxis::None {
                                obj.rotation_type = SubsysRotationType::None
                            } else if obj.rotation_type == SubsysRotationType::None {
                                obj.rotation_type = SubsysRotationType::Regular
                            }
                        }
                    });

                    ui.vertical(|ui| {
                        if selected_id.map_or(false, |id| self.model.warnings.contains(&Warning::SubObjectTranslationInvalidVersion(id))) {
                            UiState::set_widget_color(ui, WARNING_YELLOW);
                            ui.label("Translation Axis:");
                            UiState::reset_widget_color(ui);
                        } else {
                            ui.label("Translation Axis:");
                        }
                        ui.visuals_mut().widgets = Widgets::default();
                        let old_val = *trans_axis;
                        let disabled_text = "Version must be 2301 or later";
                        ui.add_enabled_ui(
                            selected_id.is_some() && (self.model.version >= pof::Version::V23_01 || *trans_axis != SubsysTranslationAxis::None),
                            |ui| {
                                ui.radio_value(trans_axis, SubsysTranslationAxis::None, "None")
                                    .on_disabled_hover_text(disabled_text);
                                ui.radio_value(trans_axis, SubsysTranslationAxis::X, "X-axis")
                                    .on_disabled_hover_text(disabled_text);
                                ui.radio_value(trans_axis, SubsysTranslationAxis::Y, "Y-axis")
                                    .on_disabled_hover_text(disabled_text);
                                ui.radio_value(trans_axis, SubsysTranslationAxis::Z, "Z-axis")
                                    .on_disabled_hover_text(disabled_text);
                                ui.radio_value(trans_axis, SubsysTranslationAxis::Other, "Other")
                                    .on_disabled_hover_text(disabled_text);
                            },
                        );
                        if old_val != *trans_axis {
                            let obj = &mut self.model.sub_objects[selected_id.unwrap()];
                            obj.translation_axis = *trans_axis;
                            if *trans_axis == SubsysTranslationAxis::None {
                                obj.translation_type = SubsysTranslationType::None
                            } else if obj.translation_type == SubsysTranslationType::None {
                                obj.translation_type = SubsysTranslationType::Regular
                            }
                            self.model
                                .recheck_warnings(One(Warning::SubObjectTranslationInvalidVersion(selected_id.unwrap())))
                        }
                    });
                });

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

                // Semantic name links ================================================================

                if let Some(id) = selected_id {
                    let subobj = &self.model.sub_objects[id];
                    if !subobj.name_links.is_empty() {
                        ui.separator();

                        let mut has_live_debris = false;
                        let mut has_detail_level = false;
                        for link in &subobj.name_links {
                            match *link {
                                pof::NameLink::DestroyedVersion(destroyed_id) => {
                                    ui.horizontal_wrapped(|ui| {
                                        ui.label(RichText::new(format!("Has a destroyed version:")).weak().color(Color32::LIGHT_RED));
                                        if ui
                                            .button(RichText::new(&self.model.sub_objects[destroyed_id].name).weak().color(Color32::LIGHT_RED))
                                            .clicked()
                                        {
                                            select_new_tree_val!(TreeValue::SubObjects(SubObjectTreeValue::SubObject(destroyed_id)));
                                        }
                                    });
                                }
                                pof::NameLink::DestroyedVersionOf(intact_id) => {
                                    ui.horizontal_wrapped(|ui| {
                                        ui.label(RichText::new(format!("Is the destroyed version of: ")).weak().color(Color32::LIGHT_RED));
                                        if ui
                                            .button(RichText::new(&self.model.sub_objects[intact_id].name).weak().color(Color32::LIGHT_RED))
                                            .clicked()
                                        {
                                            select_new_tree_val!(TreeValue::SubObjects(SubObjectTreeValue::SubObject(intact_id)));
                                        }
                                    });
                                }
                                pof::NameLink::LiveDebris(_) => has_live_debris = true,
                                pof::NameLink::LiveDebrisOf(debris_parent_id) => {
                                    ui.horizontal_wrapped(|ui| {
                                        ui.label(RichText::new(format!("Is a debris object of: ")).weak().color(LIGHT_ORANGE));
                                        if ui
                                            .button(RichText::new(&self.model.sub_objects[debris_parent_id].name).weak().color(LIGHT_ORANGE))
                                            .clicked()
                                        {
                                            select_new_tree_val!(TreeValue::SubObjects(SubObjectTreeValue::SubObject(debris_parent_id)));
                                        }
                                    });
                                }
                                pof::NameLink::DetailLevel(..) => has_detail_level = true,
                                pof::NameLink::DetailLevelOf(detail_parent_id, _) => {
                                    ui.horizontal_wrapped(|ui| {
                                        ui.label(RichText::new(format!("Is a detail object of: ")).weak().color(LIGHT_BLUE));
                                        if ui
                                            .button(RichText::new(&self.model.sub_objects[detail_parent_id].name).weak().color(LIGHT_BLUE))
                                            .clicked()
                                        {
                                            select_new_tree_val!(TreeValue::SubObjects(SubObjectTreeValue::SubObject(detail_parent_id)));
                                        }
                                    });
                                }
                            }
                        }

                        if has_live_debris {
                            ui.label(RichText::new(format!("Has sub-debris objects:")).weak().color(LIGHT_ORANGE));
                            for link in &subobj.name_links {
                                if let pof::NameLink::LiveDebris(id) = *link {
                                    if ui
                                        .button(RichText::new(&self.model.sub_objects[id].name).weak().color(LIGHT_ORANGE))
                                        .clicked()
                                    {
                                        select_new_tree_val!(TreeValue::SubObjects(SubObjectTreeValue::SubObject(id)));
                                    }
                                }
                            }
                        }

                        if has_detail_level {
                            ui.label(RichText::new(format!("Has detail level objects:")).weak().color(LIGHT_BLUE));
                            for link in &subobj.name_links {
                                if let pof::NameLink::DetailLevel(id, _) = *link {
                                    if ui
                                        .button(RichText::new(&self.model.sub_objects[id].name).weak().color(LIGHT_BLUE))
                                        .clicked()
                                    {
                                        select_new_tree_val!(TreeValue::SubObjects(SubObjectTreeValue::SubObject(id)));
                                    }
                                }
                            }
                        }
                    }
                }

                // Misc stats ================================================================

                ui.separator();

                if let Some(id) = selected_id {
                    ui.label(RichText::new(format!("Id: {:?}", self.model.sub_objects[id].obj_id)).weak());
                    let mut vert_string = RichText::new(format!("Vertices: {}", self.model.sub_objects[id].bsp_data.verts.len())).weak();
                    if self.model.errors.contains(&Error::TooManyVerts(id)) {
                        vert_string = vert_string.color(ERROR_RED);
                    }
                    let mut norm_string = RichText::new(format!("Normals: {}", self.model.sub_objects[id].bsp_data.norms.len())).weak();
                    if self.model.errors.contains(&Error::TooManyNorms(id)) {
                        norm_string = norm_string.color(ERROR_RED);
                    }
                    ui.label(vert_string);
                    ui.label(norm_string);
                }
            }
            PropertiesPanel::Texture { texture_name } => {
                ui.heading("Textures");

                ui.separator();

                if ui.button("🗐 Merge Duplicates").clicked() {
                    merge_duplicate_textures = true;
                }

                if ui.button("🔃 Reload").clicked() {
                    reload_textures = true;
                }

                ui.separator();

                let tex = if let TreeValue::Textures(TextureTreeValue::Texture(tex)) = self.ui_state.tree_view_selection {
                    Some(&mut self.model.textures[tex.0 as usize])
                } else {
                    None
                };

                ui.label("Texture Name:");
                if UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, tex, texture_name).changed()
                    && self.model.untextured_idx.is_some()
                    && self.ui_state.tree_view_selection == TreeValue::Textures(TextureTreeValue::Texture(self.model.untextured_idx.unwrap()))
                {
                    self.model.untextured_idx = None;
                    self.model.recheck_warnings(One(Warning::UntexturedPolygons));
                }

                ui.add_space(5.0);
                if TreeValue::Textures(TextureTreeValue::tex(self.model.untextured_idx)) == self.ui_state.tree_view_selection {
                    ui.label("If this is intentional, you may prefer \"invisible\", which FSO will ignore.");
                }
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
                    TreeValue::Thrusters(ThrusterTreeValue::Bank(bank)) => (Some(bank), None),
                    TreeValue::Thrusters(ThrusterTreeValue::BankPoint(bank, point)) => (Some(bank), Some(point)),
                    _ => (None, None),
                };

                let bank_idx_response = UiState::list_manipulator_widget(ui, bank_num, Some(self.model.thruster_banks.len()), "Bank");

                ui.add_space(10.0);

                ui.horizontal(|ui| {
                    ui.label("Engine Subsystem:");
                    if let Some(bank) = bank_num {
                        if self.model.warnings.contains(&Warning::ThrusterPropertiesInvalidVersion(bank)) {
                            UiState::set_widget_color(ui, WARNING_YELLOW);
                        }
                        if ui.text_edit_singleline(engine_subsys_string).changed() {
                            pof::properties_update_field(&mut self.model.thruster_banks[bank].properties, "$engine_subsystem", engine_subsys_string);
                            self.model.recheck_warnings(One(Warning::ThrusterPropertiesTooLong(bank)));
                            self.model.recheck_warnings(One(Warning::ThrusterPropertiesInvalidVersion(bank)));
                        }
                        UiState::reset_widget_color(ui);
                    } else {
                        ui.add_enabled(false, egui::TextEdit::multiline(&mut blank_string).desired_rows(1));
                    }
                });

                CollapsingHeader::new("Properties Raw").show(ui, |ui| {
                    if let Some(bank) = bank_num {
                        if self.model.warnings.contains(&Warning::ThrusterPropertiesInvalidVersion(bank)) {
                            UiState::set_widget_color(ui, WARNING_YELLOW);
                        }
                        if ui
                            .add(egui::TextEdit::multiline(&mut self.model.thruster_banks[bank].properties).desired_rows(1))
                            .changed()
                        {
                            self.model.recheck_warnings(One(Warning::ThrusterPropertiesTooLong(bank)));
                            self.model.recheck_warnings(One(Warning::ThrusterPropertiesInvalidVersion(bank)));
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

                let (pos, norm, radius) = if let TreeValue::Thrusters(ThrusterTreeValue::BankPoint(bank, point)) = self.ui_state.tree_view_selection {
                    let ThrusterGlow { position, normal, radius } = &mut self.model.thruster_banks[bank].glows[point];
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
                    let new_idx = response.get_new_ui_idx(&self.model.thruster_banks);

                    undo_history
                        .apply(&mut self.model, UndoAction::IxBAction(IndexingButtonsAction::ThrusterBanks(response)))
                        .unwrap();

                    self.model.recheck_warnings(All); //FIX

                    select_new_tree_val!(TreeValue::Thrusters(ThrusterTreeValue::bank(new_idx)));
                } else if let Some(response) = point_idx_response {
                    let new_idx = response.get_new_ui_idx(&self.model.thruster_banks[bank_num.unwrap()].glows);

                    undo_history
                        .apply(&mut self.model, UndoAction::IxBAction(IndexingButtonsAction::ThrusterBankPoints(bank_num.unwrap(), response)))
                        .unwrap();

                    select_new_tree_val!(TreeValue::Thrusters(ThrusterTreeValue::bank_point(bank_num.unwrap(), new_idx)));
                }
            }
            PropertiesPanel::Weapon { position_string, normal_string, offset_string } => {
                let (mut weapon_system, bank_num, point_num) = match self.ui_state.tree_view_selection {
                    TreeValue::Weapons(WeaponTreeValue::Header) => {
                        ui.heading("Weapons");
                        (None, None, None)
                    }
                    TreeValue::Weapons(WeaponTreeValue::PriHeader) => {
                        ui.heading("Primary Weapons");
                        (Some((&mut self.model.pof_model.primary_weps, true)), None, None)
                    }
                    TreeValue::Weapons(WeaponTreeValue::PriBank(bank)) => {
                        ui.heading("Primary Weapons");
                        (Some((&mut self.model.pof_model.primary_weps, true)), Some(bank), None)
                    }
                    TreeValue::Weapons(WeaponTreeValue::PriBankPoint(bank, point)) => {
                        ui.heading("Primary Weapons");
                        (Some((&mut self.model.pof_model.primary_weps, true)), Some(bank), Some(point))
                    }
                    TreeValue::Weapons(WeaponTreeValue::SecHeader) => {
                        ui.heading("Secondary Weapons");
                        (Some((&mut self.model.pof_model.secondary_weps, false)), None, None)
                    }
                    TreeValue::Weapons(WeaponTreeValue::SecBank(bank)) => {
                        ui.heading("Secondary Weapons");
                        (Some((&mut self.model.pof_model.secondary_weps, false)), Some(bank), None)
                    }
                    TreeValue::Weapons(WeaponTreeValue::SecBankPoint(bank, point)) => {
                        ui.heading("Secondary Weapons");
                        (Some((&mut self.model.pof_model.secondary_weps, false)), Some(bank), Some(point))
                    }
                    _ => {
                        unreachable!();
                    }
                };
                let weapon_selection = if let TreeValue::Weapons(selection) = self.ui_state.tree_view_selection {
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
                    if let TreeValue::Weapons(WeaponTreeValue::PriBankPoint(bank, point) | WeaponTreeValue::SecBankPoint(bank, point)) =
                        self.ui_state.tree_view_selection
                    {
                        let WeaponHardpoint { position, normal, offset } = &mut weapon_system.as_mut().unwrap().0[bank][point];
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
                    self.model.pof_model.warnings.contains(&Warning::WeaponOffsetInvalidVersion {
                        primary: weapon_selection.is_primary(),
                        bank: bank_num.unwrap_or_default(),
                        point: point_num.unwrap_or_default(),
                    }),
                    offset,
                    offset_string,
                )
                .changed();

                if let Some(response) = bank_idx_response {
                    let (weapon_system, is_primary) = weapon_system.unwrap();
                    let new_idx = response.get_new_ui_idx(weapon_system);

                    if is_primary {
                        undo_history
                            .apply(&mut self.model, UndoAction::IxBAction(IndexingButtonsAction::PrimaryBanks(response)))
                            .unwrap();
                    } else {
                        undo_history
                            .apply(&mut self.model, UndoAction::IxBAction(IndexingButtonsAction::SecondaryBanks(response)))
                            .unwrap();
                    }

                    self.model.recheck_warnings(All); // FIX

                    select_new_tree_val!(TreeValue::Weapons(WeaponTreeValue::bank(is_primary, new_idx)));
                } else if let Some(response) = point_idx_response {
                    let (weapon_system, is_primary) = weapon_system.unwrap();
                    let new_idx = response.get_new_ui_idx(&weapon_system[bank_num.unwrap()]);

                    if is_primary {
                        undo_history
                            .apply(&mut self.model, UndoAction::IxBAction(IndexingButtonsAction::PrimaryBankPoints(bank_num.unwrap(), response)))
                            .unwrap();
                    } else {
                        undo_history
                            .apply(&mut self.model, UndoAction::IxBAction(IndexingButtonsAction::SecondaryBankPoints(bank_num.unwrap(), response)))
                            .unwrap();
                    }

                    self.model.recheck_warnings(All); // FIX

                    select_new_tree_val!(TreeValue::Weapons(WeaponTreeValue::bank_point(is_primary, bank_num.unwrap(), new_idx)));
                }

                if offset_changed {
                    self.model.recheck_warnings(One(Warning::WeaponOffsetInvalidVersion {
                        primary: weapon_selection.is_primary(),
                        bank: bank_num.unwrap(),
                        point: point_num.unwrap(),
                    }));
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
                    TreeValue::DockingBays(DockingTreeValue::Bay(bay)) => Some(bay),
                    _ => None,
                };

                let bay_idx_response = UiState::list_manipulator_widget(ui, bay_num, Some(self.model.docking_bays.len()), "Bay");

                ui.add_space(10.0);

                ui.horizontal(|ui| {
                    ui.label("Name:");
                    if let Some(bay) = bay_num {
                        if ui.text_edit_singleline(name_string).changed() {
                            pof::properties_update_field(&mut self.model.docking_bays[bay].properties, "$name", name_string);
                            self.model.recheck_warnings(One(Warning::DockingBayNameTooLong(bay)));
                            self.model.recheck_warnings(One(Warning::DockingBayPropertiesTooLong(bay)));
                        }
                    } else {
                        ui.add_enabled(false, egui::TextEdit::singleline(&mut blank_string));
                    }
                });

                let mut subobj_names_list = vec!["None".to_string()];

                let mut warning_idx = None;
                if bay_num.map_or(false, |idx| self.model.warnings.contains(&Warning::InvalidDockParentSubmodel(idx))) {
                    // this bay has an invalid parent object, so add whatever its name is to the list
                    subobj_names_list.push(
                        pof::properties_get_field(&self.model.docking_bays[bay_num.unwrap()].properties, "$parent_submodel")
                            .unwrap()
                            .to_string(),
                    );
                    warning_idx = Some(1);
                }

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

                if let Some(new_subobj) =
                    UiState::subobject_combo_box(ui, &subobj_names_list, &mut parent_id, bay_num, "Parent Object", None, warning_idx)
                {
                    pof::properties_update_field(
                        &mut self.model.docking_bays[bay_num.unwrap()].properties,
                        "$parent_submodel",
                        &subobj_names_list[new_subobj],
                    );
                    self.model.recheck_warnings(One(Warning::DockingBayPropertiesTooLong(bay_num.unwrap())));
                    self.model.recheck_warnings(One(Warning::InvalidDockParentSubmodel(bay_num.unwrap())));
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
                        self.model.recheck_warnings(One(Warning::DockingBayWithoutPath(bay_num.unwrap())));
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
                            self.model.recheck_warnings(One(Warning::DockingBayNameTooLong(bay)));
                            self.model.recheck_warnings(One(Warning::DockingBayPropertiesTooLong(bay)));
                            self.model.recheck_warnings(One(Warning::InvalidDockParentSubmodel(bay_num.unwrap())));
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
                    *uvec_ang = bay.get_uvec_angle().to_degrees() % 360.0
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
                    let new_idx = response.get_new_ui_idx(&self.model.docking_bays);

                    undo_history
                        .apply(&mut self.model, UndoAction::IxBAction(IndexingButtonsAction::DockingBays(response)))
                        .unwrap();

                    self.model.recheck_warnings(All); // FIX

                    select_new_tree_val!(TreeValue::DockingBays(DockingTreeValue::bay(new_idx)));
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
                    TreeValue::Glows(GlowTreeValue::Bank(bank)) => (Some(bank), None),
                    TreeValue::Glows(GlowTreeValue::BankPoint(bank, point)) => (Some(bank), Some(point)),
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
                        self.model.recheck_warnings(One(Warning::GlowBankPropertiesTooLong(bank)));
                    }
                } else {
                    ui.add_enabled(false, egui::TextEdit::singleline(&mut blank_string).desired_rows(1));
                }

                let subobj_names_list = self.model.get_subobj_names();

                if let Some(new_subobj) = UiState::subobject_combo_box(ui, &subobj_names_list, attached_subobj_idx, bank_num, "SubObject", None, None)
                {
                    self.model.glow_banks[bank_num.unwrap()].obj_parent = ObjectId(new_subobj as u32);
                }

                let (disp_time, on_time, off_time, lod, glow_type) =
                    if let TreeValue::Glows(GlowTreeValue::BankPoint(bank, _)) = self.ui_state.tree_view_selection {
                        let GlowPointBank { disp_time, on_time, off_time, lod, glow_type, .. } = &mut self.model.glow_banks[bank];

                        (Some(disp_time), Some(on_time), Some(off_time), Some(lod), Some(glow_type))
                    } else if let TreeValue::Glows(GlowTreeValue::Bank(bank)) = self.ui_state.tree_view_selection {
                        let GlowPointBank { disp_time, on_time, off_time, lod, glow_type, .. } = &mut self.model.glow_banks[bank];

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
                            self.model.recheck_warnings(One(Warning::GlowBankPropertiesTooLong(bank)));
                        }
                    } else {
                        ui.add_enabled(false, egui::TextEdit::multiline(&mut String::new()).desired_rows(1));
                    }
                });

                ui.separator();

                let glow_points = if let TreeValue::Glows(GlowTreeValue::BankPoint(bank, _)) = self.ui_state.tree_view_selection {
                    Some(&mut self.model.glow_banks[bank].glow_points)
                } else if let TreeValue::Glows(GlowTreeValue::Bank(bank)) = self.ui_state.tree_view_selection {
                    Some(&mut self.model.glow_banks[bank].glow_points)
                } else {
                    None
                };

                let point_idx_response = UiState::list_manipulator_widget(ui, point_num, glow_points.map(|list| list.len()), "Point");

                let (pos, norm, radius) = if let TreeValue::Glows(GlowTreeValue::BankPoint(bank, point)) = self.ui_state.tree_view_selection {
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
                    let new_idx = response.get_new_ui_idx(&self.model.glow_banks);

                    undo_history
                        .apply(&mut self.model, UndoAction::IxBAction(IndexingButtonsAction::GlowBanks(response)))
                        .unwrap();

                    self.model.recheck_warnings(All); // FIX
                    select_new_tree_val!(TreeValue::Glows(GlowTreeValue::bank(new_idx)));
                } else if let Some(response) = point_idx_response {
                    let new_idx = response.get_new_ui_idx(&self.model.glow_banks[bank_num.unwrap()].glow_points);

                    undo_history
                        .apply(&mut self.model, UndoAction::IxBAction(IndexingButtonsAction::GlowBankPoints(bank_num.unwrap(), response)))
                        .unwrap();

                    self.model.recheck_warnings(All); // FIX
                    select_new_tree_val!(TreeValue::Glows(GlowTreeValue::bank_point(bank_num.unwrap(), new_idx)));
                }
            }
            PropertiesPanel::SpecialPoint { radius_string, position_string, name_string } => {
                ui.heading("Special Point");
                ui.separator();

                let point_num = match self.ui_state.tree_view_selection {
                    TreeValue::SpecialPoints(SpecialPointTreeValue::Point(point)) => Some(point),
                    _ => None,
                };

                let spec_point_idx_response = UiState::list_manipulator_widget(ui, point_num, Some(self.model.special_points.len()), "Point");

                ui.add_space(10.0);

                ui.horizontal(|ui| {
                    ui.label("Name:");
                    if let Some(point) = point_num {
                        if ui.add(egui::TextEdit::singleline(&mut self.model.special_points[point].name)).changed() {
                            self.model.recheck_warnings(One(Warning::SpecialPointNameTooLong(point)));
                        }
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
                            self.model.recheck_warnings(One(Warning::SpecialPointPropertiesTooLong(point)));
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
                            self.model.recheck_warnings(One(Warning::SpecialPointPropertiesTooLong(point)));
                        }
                    } else {
                        ui.add_enabled(false, egui::TextEdit::multiline(&mut String::new()).desired_rows(1));
                    }
                });

                ui.separator();

                ui.add_space(10.0);

                let (pos, radius) = if let TreeValue::SpecialPoints(SpecialPointTreeValue::Point(point)) = self.ui_state.tree_view_selection {
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
                    let new_idx = response.get_new_ui_idx(&self.model.special_points);

                    undo_history
                        .apply(&mut self.model, UndoAction::IxBAction(IndexingButtonsAction::SpecialPoints(response)))
                        .unwrap();

                    self.model.recheck_warnings(All); // FIX

                    select_new_tree_val!(TreeValue::SpecialPoints(SpecialPointTreeValue::point(new_idx)));
                }
            }
            PropertiesPanel::Turret { position_string, normal_string, base_idx } => {
                ui.heading("Turret");
                ui.separator();

                let (turret_num, point_num) = match self.ui_state.tree_view_selection {
                    TreeValue::Turrets(TurretTreeValue::Turret(turret)) => (Some(turret), None),
                    TreeValue::Turrets(TurretTreeValue::TurretPoint(turret, point)) => (Some(turret), Some(point)),
                    _ => (None, None),
                };

                // no subobjects = no turrets allowed
                let turrets_len_opt = (!self.model.sub_objects.is_empty()).then(|| self.model.turrets.len());
                let turret_idx_response = UiState::list_manipulator_widget(ui, turret_num, turrets_len_opt, "Turret");

                ui.add_space(10.0);
                let subobj_names_list = self.model.get_subobj_names();

                if let Some(new_subobj) = UiState::subobject_combo_box(ui, &subobj_names_list, base_idx, turret_num, "Base object", None, None) {
                    self.model.turrets[turret_num.unwrap()].base_obj = ObjectId(new_subobj as u32);
                    self.model.recheck_errors(One(Error::InvalidTurretGunSubobject(turret_num.unwrap())));
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
                    if self.model.errors.contains(&Error::InvalidTurretGunSubobject(num)) {
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
                    UiState::subobject_combo_box(ui, &gun_subobj_names_list, &mut gun_subobj_idx, turret_num, "Gun object", error_idx, None)
                {
                    // the unwraps are ok here, if it were none, the combo box would be un-interactable
                    self.model.turrets[turret_num.unwrap()].gun_obj = gun_subobj_ids_list[new_idx];
                    self.model.recheck_errors(One(Error::InvalidTurretGunSubobject(turret_num.unwrap())));
                    self.ui_state.viewport_3d_dirty = true;
                }

                let norm = if let TreeValue::Turrets(TurretTreeValue::Turret(turret) | TurretTreeValue::TurretPoint(turret, _)) =
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

                let pos = if let TreeValue::Turrets(TurretTreeValue::TurretPoint(turret, point)) = self.ui_state.tree_view_selection {
                    Some(&mut self.model.turrets[turret].fire_points[point])
                } else {
                    None
                };

                ui.label("Position:");
                UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, pos, position_string);

                if let Some(response) = turret_idx_response {
                    let new_idx = response.get_new_ui_idx(&self.model.turrets);

                    undo_history
                        .apply(&mut self.model, UndoAction::IxBAction(IndexingButtonsAction::Turrets(response)))
                        .unwrap();

                    self.model.recheck_errors(All); // FIX
                    self.model.recheck_warnings(All); // FIX

                    select_new_tree_val!(TreeValue::Turrets(TurretTreeValue::turret(new_idx)));
                } else if let Some(response) = point_idx_response {
                    let new_idx = response.get_new_ui_idx(&self.model.turrets[turret_num.unwrap()].fire_points);

                    undo_history
                        .apply(&mut self.model, UndoAction::IxBAction(IndexingButtonsAction::TurretPoints(turret_num.unwrap(), response)))
                        .unwrap();

                    self.model.recheck_errors(All); // FIX
                    self.model.recheck_warnings(All); // FIX

                    select_new_tree_val!(TreeValue::Turrets(TurretTreeValue::turret_point(turret_num.unwrap(), new_idx)));
                }
            }
            PropertiesPanel::Path { name, parent_string, position_string, radius_string } => {
                ui.heading("Path");
                ui.separator();

                let (path_num, point_num) = match self.ui_state.tree_view_selection {
                    TreeValue::Paths(PathTreeValue::Path(path)) => (Some(path), None),
                    TreeValue::Paths(PathTreeValue::PathPoint(path, point)) => (Some(path), Some(point)),
                    _ => (None, None),
                };

                let path_idx_response = UiState::list_manipulator_widget(ui, path_num, Some(self.model.paths.len()), "Bank");

                ui.add_space(10.0);

                ui.label("Name:");
                if let Some(num) = path_num {
                    let old_name = self.model.paths[num].name.clone();
                    if ui
                        .add(egui::TextEdit::multiline(&mut self.model.paths[num].name).desired_rows(1))
                        .changed()
                    {
                        self.model.recheck_warnings(One(Warning::DuplicatePathName(old_name)));
                        self.model
                            .pof_model
                            .recheck_warnings(One(Warning::DuplicatePathName(self.model.pof_model.paths[num].name.clone())));
                        self.model.recheck_warnings(One(Warning::PathNameTooLong(num)));
                    };
                } else {
                    ui.add_enabled(false, egui::TextEdit::multiline(name).desired_rows(1));
                }

                ui.label("Parent:");
                if let Some(num) = path_num {
                    ui.add(egui::TextEdit::multiline(&mut self.model.paths[num].parent).desired_rows(1))
                        .changed();
                } else {
                    ui.add_enabled(false, egui::TextEdit::multiline(parent_string).desired_rows(1));
                }

                ui.separator();

                let point_idx_response =
                    UiState::list_manipulator_widget(ui, point_num, path_num.map(|num| self.model.paths[num].points.len()), "Point");

                ui.add_space(10.0);

                let (radius, pos) = if let TreeValue::Paths(PathTreeValue::PathPoint(path, point)) = self.ui_state.tree_view_selection {
                    let PathPoint { position, radius, .. } = &mut self.model.paths[path].points[point];
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
                    }
                    let new_idx = response.get_new_ui_idx(&self.model.paths);

                    undo_history
                        .apply(&mut self.model, UndoAction::IxBAction(IndexingButtonsAction::Paths(response)))
                        .unwrap();

                    self.model.recheck_warnings(All); // FIX

                    select_new_tree_val!(TreeValue::Paths(PathTreeValue::path(new_idx)));
                } else if let Some(response) = point_idx_response {
                    let new_idx = response.get_new_ui_idx(&self.model.paths[path_num.unwrap()].points);

                    undo_history
                        .apply(&mut self.model, UndoAction::IxBAction(IndexingButtonsAction::PathPoints(path_num.unwrap(), response)))
                        .unwrap();

                    select_new_tree_val!(TreeValue::Paths(PathTreeValue::path_point(path_num.unwrap(), new_idx)));
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

                let (lod, offset) = if let TreeValue::Insignia(InsigniaTreeValue::Insignia(idx)) = self.ui_state.tree_view_selection {
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
                    TreeValue::EyePoints(EyeTreeValue::EyePoint(point)) => Some(point),
                    _ => None,
                };

                // no subobjects = no eye points allowed
                let eye_points_len_opt = (!self.model.sub_objects.is_empty()).then(|| self.model.eye_points.len());
                let eye_idx_response = UiState::list_manipulator_widget(ui, eye_num, eye_points_len_opt, "Eye Point");

                ui.add_space(10.0);

                ui.add_enabled_ui(eye_num.is_some(), |ui| {
                    if let Some(num) = eye_num {
                        let mut name_list = self.model.get_subobj_names();
                        if self.model.eye_points[num].attached_subobj.is_none() {
                            name_list.push("None".to_string());
                        }

                        egui::ComboBox::from_label("Attached submodel")
                            .show_index(ui, attached_subobj_idx, name_list.len(), |i| name_list[i].to_owned());

                        if *attached_subobj_idx < self.model.sub_objects.len() {
                            self.model.eye_points[num].attached_subobj = Some(ObjectId(*attached_subobj_idx as u32));
                        }
                    } else {
                        egui::ComboBox::from_label("Attached submodel").show_index(ui, attached_subobj_idx, 1, |_| format!(""));
                    }
                });

                ui.separator();

                ui.add_space(10.0);

                let (pos, norm) = if let TreeValue::EyePoints(EyeTreeValue::EyePoint(point)) = self.ui_state.tree_view_selection {
                    let EyePoint { position, normal, .. } = &mut self.model.eye_points[point];
                    (Some(position), Some(normal))
                } else {
                    (None, None)
                };
                ui.label("Position:");
                UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, pos, position_string);
                ui.label("Normal:");
                UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, norm, normal_string);

                if let Some(response) = eye_idx_response {
                    let new_idx = response.get_new_ui_idx(&self.model.eye_points);

                    undo_history
                        .apply(&mut self.model, UndoAction::IxBAction(IndexingButtonsAction::EyePoints(response)))
                        .unwrap();

                    self.model.recheck_warnings(All); //FIX

                    select_new_tree_val!(TreeValue::EyePoints(EyeTreeValue::point(new_idx)));
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

        if merge_duplicate_textures {
            use pof::TextureId;
            let mut tex_name_map = HashMap::new();
            let mut merged_map = HashMap::new();
            let mut new_textures = vec![];
            for (i, tex) in self.model.textures.iter().enumerate() {
                if tex_name_map.contains_key(tex) {
                    merged_map.insert(TextureId(i as u32), tex_name_map[tex]);
                } else {
                    merged_map.insert(TextureId(i as u32), TextureId(tex_name_map.len() as u32));
                    tex_name_map.insert(tex, TextureId(tex_name_map.len() as u32));
                    new_textures.push(tex.clone());
                }
            }

            undo_history
                .apply(&mut self.model, UndoAction::ChangeTextures { id_map: merged_map, textures: new_textures })
                .unwrap();

            self.ui_state.properties_panel_dirty = true;
        }

        if reload_textures {
            self.load_textures();
        }

        if self.ui_state.properties_panel_dirty {
            self.sanitize_ui_state();
            self.ui_state.refresh_properties_panel(&self.model);
            self.ui_state.properties_panel_dirty = false;
        }

        if rebuild_all_buffers {
            self.rebuild_all_subobj_buffers(display);
            self.rebuild_all_insignia_buffers(display);
            self.rebuild_shield_buffer(display);
        } else {
            self.rebuild_subobj_buffers(display, buffer_ids_to_rebuild);
        }
    }
}
