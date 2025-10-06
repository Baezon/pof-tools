#![allow(clippy::unnecessary_lazy_evaluations)]
use std::collections::HashMap;
use std::f32::consts::TAU;
use std::hash::Hash;
use std::mem::swap;
use std::str::FromStr;

use egui::Align2;
use egui::{style::Widgets, text::LayoutJob, CollapsingHeader, Color32, DragValue, Label, Response, RichText, TextEdit, TextFormat, TextStyle, Ui};
use glium::glutin::surface::WindowSurface;
use glium::Display;
use nalgebra_glm::TMat4;
use pof::{
    Dock, Error, NormalVec3, ObjectId, PathId, Set::*, SubsysRotationAxis, SubsysRotationType, SubsysTranslationAxis, SubsysTranslationType, Vec3d,
    Warning,
};

use crate::Model;

use crate::ui::{
    model_action, DockingTreeValue, EyeTreeValue, GlowTreeValue, InsigniaTreeValue, PathTreeValue, PofToolsGui, SpecialPointTreeValue,
    SubObjectTreeValue, TextureTreeValue, ThrusterTreeValue, TreeValue, TurretTreeValue, UiState, UndoAction, WeaponTreeValue, ERROR_RED, LIGHT_BLUE,
    LIGHT_ORANGE, WARNING_YELLOW,
};

const NON_BREAK_SPACE: char = '\u{00A0}';

type UndoFunction = Box<dyn FnMut(&mut Model)>;
type PathFunction<T> = Box<dyn FnMut(&mut Model) -> &mut T>;

/// doesnt do much other than boxing, but the real benefit is to coerce arbitrary closure types into the type needed by the undo system
pub fn path_func<T: 'static>(val: impl FnMut(&mut Model) -> &mut T + 'static) -> PathFunction<T> {
    Box::new(val)
}

/// doesnt do much, but the real benefit is to coerce arbitrary closure types into the type needed by the undo system
pub fn parse_func<T, F: FnMut(&Model, T) -> UndoFunction>(val: F) -> F {
    val
}

/// doesnt do much other than boxing, but the real benefit is to coerce arbitrary closure types into the type needed by the undo system
pub fn undo_func(val: impl FnMut(&mut Model) + 'static) -> UndoFunction {
    Box::new(val)
}

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

    pub fn get_new_ui_idx(&self, data_vec: &[T]) -> Option<usize>
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

    /// does this response need to count as an action for the undo system
    pub fn undoable(&self) -> bool {
        !matches!(self, IndexingButtonsResponse::Switch(_))
    }
}

pub fn visibility_button(ui: &mut Ui, toggled: bool) -> Response {
    let res = ui.selectable_label(toggled, RichText::new("    ").text_style(TextStyle::Button));

    let painter = ui.painter_at(res.rect);
    let mut pos = res.rect.center();
    pos.x += 0.25;
    painter.text(
        pos,
        Align2::CENTER_CENTER,
        "👁",
        egui::TextStyle::Heading.resolve(ui.style()),
        if res.hovered() || toggled {
            ui.visuals().strong_text_color()
        } else {
            ui.visuals().text_color()
        },
    );

    ui.add_space(-3.0);

    res
}

fn text_edit_single_no_undo(ui: &mut Ui, id: impl Hash, string: &mut String) -> Response {
    let id = egui::Id::new(id);
    let text_edit = egui::TextEdit::singleline(string).id(id);
    let response = ui.add(text_edit);
    egui::TextEdit::load_state(ui.ctx(), id).unwrap().clear_undoer();
    response
}

fn text_edit_single(
    ui: &mut Ui, id: String, model: &mut Model, mut string_path: PathFunction<String>, undo_history: &mut undo::History<UndoAction>,
) -> Response {
    let mut old_val = string_path(model).clone();

    let egui_id = egui::Id::new(&id);
    let text_edit = egui::TextEdit::singleline(string_path(model)).id(egui_id);
    let response = ui.add(text_edit);

    if response.changed() {
        let mut first_time = true;
        let func = Box::new(move |model: &mut Model| {
            if first_time {
                first_time = false;
            } else {
                let val = string_path(model);
                info!("Modifying: {}", id);
                swap(&mut old_val, val);
            }
        });
        let _ = undo_history.apply(model, UndoAction { function: func });
    }

    egui::TextEdit::load_state(ui.ctx(), egui_id).unwrap().clear_undoer();
    response
}

fn text_edit_multi(
    ui: &mut Ui, id: String, desired_rows: usize, model: &mut Model, mut string_path: PathFunction<String>,
    undo_history: &mut undo::History<UndoAction>,
) -> Response {
    let mut old_val = string_path(model).clone();

    let egui_id = egui::Id::new(&id);
    let text_edit = egui::TextEdit::multiline(string_path(model)).desired_rows(desired_rows).id(egui_id);
    let response = ui.add(text_edit);

    if response.changed() {
        let mut first_time = true;
        let func = Box::new(move |model: &mut Model| {
            if first_time {
                first_time = false;
            } else {
                let val = string_path(model);
                info!("Modifying: {}", id);
                swap(&mut old_val, val);
            }
        });
        let _ = undo_history.apply(model, UndoAction { function: func });
    }

    egui::TextEdit::load_state(ui.ctx(), egui_id).unwrap().clear_undoer();
    response
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
        ui: &mut Ui, current_num: Option<usize>, list: Option<&Vec<T>>, index_name: &str,
    ) -> Option<IndexingButtonsResponse<T>> {
        enum Icon {
            Arrow,
            Plus,
        }
        let list_len = list.map(|list| list.len());

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

    // model_value_edit but with a custom undo function
    #[allow(clippy::too_many_arguments)]
    fn model_value_edit_custom<T: FromStr + Clone + 'static, F: FnMut(&Model, T) -> UndoFunction>(
        id: impl Hash, viewport_3d_dirty: &mut bool, ui: &mut Ui, active_warning: bool, model_func: Option<F>, parsable_string: &mut String,
        undo_history: &mut undo::History<UndoAction>, model: &mut Model,
    ) -> Response {
        if let Some(mut model_func) = model_func {
            if parsable_string.parse::<T>().is_err() {
                ui.visuals_mut().override_text_color = Some(ERROR_RED);
            } else if active_warning {
                ui.visuals_mut().override_text_color = Some(WARNING_YELLOW);
            }

            let egui_id = egui::Id::new(id);
            let text_edit = egui::TextEdit::singleline(parsable_string).id(egui_id);
            let response = ui.add(text_edit);
            egui::TextEdit::load_state(ui.ctx(), egui_id).unwrap().clear_undoer();

            if response.changed() {
                if let Ok(new_val) = parsable_string.parse::<T>() {
                    let func = model_func(model, new_val);
                    let _ = undo_history.apply(model, UndoAction { function: func });

                    *viewport_3d_dirty = true;
                }
            }
            ui.visuals_mut().override_text_color = None;
            response
        } else {
            ui.add_enabled_ui(false, |ui| ui.text_edit_singleline(parsable_string)).inner
        }
    }

    #[allow(clippy::too_many_arguments)] // yeah its got a lot of arguments, but it is mostly called through a macro model_value_widget which elides many of them
    fn model_value_edit<T: FromStr + Clone + 'static>(
        id: String, viewport_3d_dirty: &mut bool, ui: &mut Ui, active_warning: bool, model_path: Option<PathFunction<T>>,
        parsable_string: &mut String, undo_history: &mut undo::History<UndoAction>, model: &mut Model,
    ) -> Response {
        if let Some(mut model_path) = model_path {
            if parsable_string.parse::<T>().is_err() {
                ui.visuals_mut().override_text_color = Some(ERROR_RED);
            } else if active_warning {
                ui.visuals_mut().override_text_color = Some(WARNING_YELLOW);
            }

            let egui_id = egui::Id::new(&id);
            let text_edit = egui::TextEdit::singleline(parsable_string).id(egui_id);
            let response = ui.add(text_edit);
            egui::TextEdit::load_state(ui.ctx(), egui_id).unwrap().clear_undoer();

            if response.changed() {
                if let Ok(mut new_val) = parsable_string.parse() {
                    let func = Box::new(move |model: &mut Model| {
                        let val = model_path(model);
                        info!("Modifying: {}", id);
                        swap(&mut new_val, val);
                    });
                    let _ = undo_history.apply(model, UndoAction { function: func });

                    *viewport_3d_dirty = true;
                }
            }
            ui.visuals_mut().override_text_color = None;
            response
        } else {
            ui.add_enabled_ui(false, |ui| ui.text_edit_singleline(parsable_string)).inner
        }
    }

    fn show_transform_window(
        ctx: &egui::Context, transform_window: &mut TransformWindow, text: Option<impl Into<egui::WidgetText>>,
    ) -> Option<TMat4<f32>> {
        let mut ret = None;
        let window = egui::Window::new("Transform")
            .collapsible(false)
            .resizable(false)
            .default_size((250.0, 200.0))
            .open(&mut transform_window.open)
            .anchor(egui::Align2::RIGHT_TOP, [-100.0, 100.0]);

        window.show(ctx, |ui| {
            if let Some(text) = text {
                ui.label(text);
            }

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
                        name_string: format!("{}", model.paths[path].name),
                        parent_string: format!("{}", model.paths[path].parent),
                        position_string: format!("{}", model.paths[path].points[point].position),
                        radius_string: format!("{}", model.paths[path].points[point].radius),
                    }
                }
                PathTreeValue::Path(path) => {
                    self.properties_panel = PropertiesPanel::Path {
                        name_string: format!("{}", model.paths[path].name),
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
        name_string: String,
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
            name_string: Default::default(),
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
        &mut self, ui: &mut egui::Ui, ctx: &egui::Context, display: &Display<WindowSurface>, undo_history: &mut undo::History<UndoAction>,
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

        macro_rules! model_value_widget {
            ($id:expr, $ui:expr, $warning:expr, $path:expr, $string:expr) => {
                UiState::model_value_edit($id, &mut self.ui_state.viewport_3d_dirty, $ui, $warning, $path, $string, undo_history, &mut self.model)
            };
        }

        // this is needed for blank string fields when the properties panel can't display
        // anything for that field due to an invalid tree selection
        let mut blank_string = String::new();

        let current_tree_selection = self.ui_state.tree_view_selection;

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

                let mut display_bbox = false;
                ui.horizontal(|ui| {
                    if visibility_button(ui, self.always_show_bbox).clicked() {
                        self.always_show_bbox = !self.always_show_bbox;
                    }

                    ui.label("Bounding Box:");
                });

                ui.horizontal(|ui| {
                    ui.label("Min:");
                    let response = model_value_widget!(
                        format!("{} bounding box min", current_tree_selection),
                        ui,
                        self.model.warnings.contains(&Warning::BBoxTooSmall(None)),
                        Some(path_func(|model| &mut model.header.bbox.min)),
                        bbox_min_string
                    );
                    display_bbox |= response.hovered() || response.has_focus();
                });

                ui.horizontal(|ui| {
                    ui.label("Max:");
                    let response = model_value_widget!(
                        format!("{} bounding box max", current_tree_selection),
                        ui,
                        self.model.warnings.contains(&Warning::BBoxTooSmall(None)),
                        Some(path_func(|model| &mut model.header.bbox.max)),
                        bbox_max_string
                    );
                    display_bbox |= response.hovered() || response.has_focus();
                });

                let response = ui.button("Recalculate");
                if response.clicked() {
                    let mut new_bbox = self.model.recalc_bbox();
                    model_action(
                        undo_history,
                        &mut self.model,
                        undo_func(move |model| {
                            swap(&mut model.header.bbox, &mut new_bbox);
                        }),
                    );
                    self.ui_state.properties_panel_dirty = true;
                }
                display_bbox |= response.hovered() || response.has_focus();

                self.ui_state.display_bbox = self.always_show_bbox || display_bbox;

                let bbox = &self.model.header.bbox;
                ui.horizontal_wrapped(|ui| {
                    ui.label(format!("Width:{NON_BREAK_SPACE}{:.1}", bbox.x_width()));
                    ui.label(format!("Height:{NON_BREAK_SPACE}{:.1}", bbox.y_height()));
                    ui.label(format!("Length:{NON_BREAK_SPACE}{:.1}", bbox.z_length()));
                });

                ui.separator();

                let mut display_radius = false;
                ui.horizontal(|ui| {
                    if visibility_button(ui, self.always_show_radius).clicked() {
                        self.always_show_radius = !self.always_show_radius;
                    }

                    ui.label("Radius:");
                });

                let response = model_value_widget!(
                    format!("{} radius", current_tree_selection),
                    ui,
                    self.model.warnings.contains(&Warning::RadiusTooSmall(None)),
                    Some(path_func(|model| &mut model.header.max_radius)),
                    radius_string
                );
                display_radius |= response.hovered() || response.has_focus();

                let response = ui.button("Recalculate");
                if response.clicked() {
                    let mut new_radius = self.model.recalc_radius();

                    model_action(
                        undo_history,
                        &mut self.model,
                        undo_func(move |model| {
                            swap(&mut model.header.max_radius, &mut new_radius);
                        }),
                    );
                    self.ui_state.properties_panel_dirty = true;
                }
                display_radius |= response.hovered() || response.has_focus();

                self.ui_state.display_radius = self.always_show_radius || display_radius;

                ui.separator();

                ui.horizontal(|ui| {
                    ui.add(egui::Label::new("Mass:"));
                    if ui.button("Recalculate").clicked() {
                        let mut mass = self.model.recalc_mass();
                        model_action(
                            undo_history,
                            &mut self.model,
                            undo_func(move |model| {
                                swap(&mut model.header.mass, &mut mass);
                            }),
                        );
                        self.ui_state.properties_panel_dirty = true;
                    }
                });
                model_value_widget!(
                    format!("{} mass", current_tree_selection),
                    ui,
                    false,
                    Some(path_func(|model| &mut model.header.mass)),
                    mass_string
                );

                ui.horizontal(|ui| {
                    ui.add(egui::Label::new("Moment of Inertia:"));
                    if ui.button("Recalculate").clicked() {
                        if let Some(mut moi) = self.model.recalc_moi() {
                            model_action(
                                undo_history,
                                &mut self.model,
                                undo_func(move |model| {
                                    swap(&mut model.header.moment_of_inertia, &mut moi);
                                }),
                            );
                            self.ui_state.properties_panel_dirty = true;
                        }
                    }
                });
                model_value_widget!(
                    format!("{} moment of inertia rvec", current_tree_selection),
                    ui,
                    false,
                    Some(path_func(|model| &mut model.header.moment_of_inertia.rvec)),
                    moir_string
                );
                model_value_widget!(
                    format!("{} moment of inertia uvec", current_tree_selection),
                    ui,
                    false,
                    Some(path_func(|model| &mut model.header.moment_of_inertia.uvec)),
                    moiu_string
                );
                model_value_widget!(
                    format!("{} moment of inertia fvec", current_tree_selection),
                    ui,
                    false,
                    Some(path_func(|model| &mut model.header.moment_of_inertia.fvec)),
                    moif_string
                );

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
                    let mut new_list = self.model.header.detail_levels.clone();
                    if let Some(new_id) = id_opt {
                        // change to a new idx
                        if level == new_list.len() {
                            // add a new detail level
                            new_list.push(new_id);
                        } else if let Some(swapped_level) = new_list.iter().position(|&id| id == new_id) {
                            // swap with an existing level
                            let swapped_id = new_list[level];
                            new_list[level] = new_id;
                            new_list[swapped_level] = swapped_id;
                        } else {
                            new_list[level] = new_id;
                        }
                    } else {
                        // Remove a detail level
                        // No holes allowed, so truncate
                        new_list.truncate(level);
                    }

                    model_action(
                        undo_history,
                        &mut self.model,
                        undo_func(move |model| {
                            swap(&mut model.header.detail_levels, &mut new_list);
                        }),
                    );
                }

                ui.separator();

                if ui.add(egui::Button::new("Transform")).clicked() {
                    transform_window.open = true;
                }

                let mut text = LayoutJob::default();
                text.append(
                    "This will affect all subobjects, all weapon points, docking points etc. and ",
                    0.0,
                    TextFormat {
                        font_id: TextStyle::Body.resolve(ui.style()),
                        ..Default::default()
                    },
                );
                text.append(
                    "can't be undone.",
                    0.0,
                    TextFormat {
                        font_id: TextStyle::Body.resolve(ui.style()),
                        color: WARNING_YELLOW,
                        ..Default::default()
                    },
                );
                if let Some(matrix) = UiState::show_transform_window(ctx, transform_window, Some(text)) {
                    // this is way too complicated to undo...
                    undo_history.clear();
                    self.model.apply_transform(&matrix);
                    self.ui_state.viewport_3d_dirty = true;
                    self.ui_state.properties_panel_dirty = true;
                    self.model.recheck_warnings(One(Warning::Detail0NonZeroOffset));
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

                let selected_id = if let TreeValue::SubObjects(SubObjectTreeValue::SubObject(id)) = current_tree_selection {
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

                    if text_edit_single(
                        ui,
                        "subobj name".to_string(),
                        &mut self.model,
                        path_func(move |model| &mut model.sub_objects[id].name),
                        undo_history,
                    )
                    .changed()
                    {
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
                        let id = selected_id.unwrap();

                        model_action(
                            undo_history,
                            &mut self.model,
                            undo_func(move |model| {
                                model.sub_objects[id].is_debris_model = !model.sub_objects[id].is_debris_model;
                            }),
                        );
                    }

                    UiState::reset_widget_color(ui);
                });

                ui.separator();

                // Bounding Box edit ================================================================

                let mut bbox_changed = false;
                let mut display_bbox = false;
                ui.horizontal(|ui| {
                    if visibility_button(ui, self.always_show_bbox).clicked() {
                        self.always_show_bbox = !self.always_show_bbox;
                    }

                    ui.label("Bounding Box:");
                });

                ui.horizontal(|ui| {
                    ui.label("Min:");
                    let response = model_value_widget!(
                        format!("{} bounding box min", current_tree_selection),
                        ui,
                        false,
                        selected_id.map(|id| path_func(move |model| &mut model.sub_objects[id].bbox.min)),
                        bbox_min_string
                    );

                    if response.changed() {
                        bbox_changed = true;
                    }
                    display_bbox |= response.hovered() || response.has_focus();
                });

                ui.horizontal(|ui| {
                    ui.label("Max:");
                    let response = model_value_widget!(
                        format!("{} bounding box max", current_tree_selection),
                        ui,
                        false,
                        selected_id.map(|id| path_func(move |model| &mut model.sub_objects[id].bbox.max)),
                        bbox_max_string
                    );

                    if response.changed() {
                        bbox_changed = true;
                    }
                    display_bbox = response.hovered() || response.has_focus() || display_bbox;
                });

                let response = ui.add_enabled(selected_id.is_some(), egui::Button::new("Recalculate"));
                if response.clicked() {
                    let id = selected_id.unwrap();
                    let mut new_bbox = self.model.sub_objects[id].recalc_bbox();
                    model_action(
                        undo_history,
                        &mut self.model,
                        undo_func(move |model| {
                            swap(&mut model.sub_objects[id].bbox, &mut new_bbox);
                        }),
                    );
                }
                display_bbox |= response.hovered() || response.has_focus();

                self.ui_state.display_bbox = self.always_show_bbox || display_bbox;

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

                let mut display_origin = false;
                ui.horizontal_wrapped(|ui| {
                    if visibility_button(ui, self.always_show_offset).clicked() {
                        self.always_show_offset = !self.always_show_offset;
                    }

                    if self.model.header.detail_levels.first().map_or(false, |id| selected_id == Some(*id))
                        && self.model.warnings.contains(&Warning::Detail0NonZeroOffset)
                    {
                        ui.visuals_mut().override_text_color = Some(WARNING_YELLOW);
                    }
                    ui.label("Offset:");
                    ui.visuals_mut().override_text_color = None;
                });

                if let Some(id) = selected_id {
                    let only_offset = self.ui_state.move_only_offset;
                    let offset_move = parse_func(move |model: &Model, mut new_offset: Vec3d| {
                        let mut diff = model.sub_objects[id].offset - new_offset;
                        let mut new_radius = model.sub_objects[id].radius + diff.magnitude(); //this is an overestimate... maybe fix...
                        let mut new_bbox = model.sub_objects[id].bbox.shift(diff);
                        if only_offset {
                            undo_func(move |model| {
                                let subobj = &mut model.sub_objects[id];
                                swap(&mut subobj.offset, &mut new_offset);
                                swap(&mut subobj.radius, &mut new_radius);
                                swap(&mut subobj.bbox, &mut new_bbox);
                                let children: Vec<_> = subobj.children().copied().collect();
                                for id in children {
                                    model.sub_objects[id].offset += diff;
                                }
                                model.subobject_transform_matrix[id].append_translation_mut(&diff.into());

                                diff = -diff;
                                info!("Modifying: Subobject - {:?} - offset", id);
                            })
                        } else {
                            undo_func(move |model| {
                                swap(&mut model.sub_objects[id].offset, &mut new_offset);
                                info!("Modifying: Subobject - {:?} - offset", id);
                            })
                        }
                    });

                    let response = UiState::model_value_edit_custom(
                        "subobj offset",
                        &mut self.ui_state.viewport_3d_dirty,
                        ui,
                        false,
                        Some(offset_move),
                        offset_string,
                        undo_history,
                        &mut self.model,
                    );
                    if response.changed() {
                        self.model.recheck_warnings(One(Warning::Detail0NonZeroOffset));
                        self.ui_state.viewport_3d_dirty = true;
                        self.ui_state.properties_panel_dirty = true;
                    }
                    display_origin |= response.hovered() || response.has_focus();
                } else {
                    ui.add_enabled(false, TextEdit::singleline(offset_string));
                }

                let response = ui.add_enabled(selected_id.is_some(), egui::Button::new("Recalculate"));
                if response.clicked() {
                    let id = selected_id.unwrap();
                    let mut new_offset = self.model.recalc_subobj_offset(id);
                    let mut diff = self.model.sub_objects[id].offset - new_offset;
                    let mut new_radius = self.model.sub_objects[id].radius + diff.magnitude(); //this is an overestimate... maybe fix...
                    let mut new_bbox = self.model.sub_objects[id].bbox.shift(diff);
                    model_action(
                        undo_history,
                        &mut self.model,
                        undo_func(move |model| {
                            let subobj = &mut model.sub_objects[id];
                            swap(&mut subobj.offset, &mut new_offset);
                            swap(&mut subobj.radius, &mut new_radius);
                            swap(&mut subobj.bbox, &mut new_bbox);
                            let children: Vec<_> = subobj.children().copied().collect();
                            for id in children {
                                model.sub_objects[id].offset += diff;
                            }
                            model.subobject_transform_matrix[id].append_translation_mut(&diff.into());

                            diff = -diff;
                            info!("Recalculating: Subobject - {:?} - offset", id);
                        }),
                    );
                    self.ui_state.viewport_3d_dirty = true;
                    self.ui_state.properties_panel_dirty = true;
                }
                display_origin |= response.hovered() || response.has_focus();

                let response = ui
                    .add_enabled(selected_id.is_some(), egui::Checkbox::new(&mut self.ui_state.move_only_offset, "Modify Offset Only"))
                    .on_hover_text(
                        "Changes will affect only the offset/center of this subobject, all other geometry remains in place.\nThe radius will be recalculated.",
                    );
                display_origin |= response.hovered() || response.has_focus();

                self.ui_state.display_origin = display_origin || self.always_show_offset;

                ui.separator();

                // Radius edit ================================================================

                let mut display_radius = false;
                ui.horizontal(|ui| {
                    if visibility_button(ui, self.always_show_radius).clicked() {
                        self.always_show_radius = !self.always_show_radius;
                    }

                    ui.label("Radius:");
                });

                let response = model_value_widget!(
                    format!("{} radius", current_tree_selection),
                    ui,
                    self.model.warnings.contains(&Warning::RadiusTooSmall(selected_id)),
                    selected_id.map(|id| path_func(move |model| &mut model.sub_objects[id].radius)),
                    radius_string
                );
                display_radius |= response.hovered() || response.has_focus();

                if response.changed() {
                    self.model.recheck_warnings(One(Warning::RadiusTooSmall(selected_id)));
                }

                let response = ui.add_enabled(selected_id.is_some(), egui::Button::new("Recalculate"));
                if response.clicked() {
                    let id = selected_id.unwrap();
                    let mut new_radius = self.model.sub_objects[id].recalc_radius();

                    model_action(
                        undo_history,
                        &mut self.model,
                        undo_func(move |model| {
                            swap(&mut model.sub_objects[id].radius, &mut new_radius);
                        }),
                    );
                }
                display_radius |= response.hovered() || response.has_focus();

                self.ui_state.display_radius = self.always_show_radius || display_radius;

                ui.separator();

                // Transform button ================================================================

                if ui.add_enabled(selected_id.is_some(), egui::Button::new("Transform")).clicked() {
                    transform_window.open = true;
                }
                let mut text = LayoutJob::default();
                text.append(
                    "This will affect all child subobjects of this one, its turret firepoints if any and ",
                    0.0,
                    TextFormat {
                        font_id: TextStyle::Body.resolve(ui.style()),
                        ..Default::default()
                    },
                );
                text.append(
                    "can't be undone.",
                    0.0,
                    TextFormat {
                        font_id: TextStyle::Body.resolve(ui.style()),
                        color: WARNING_YELLOW,
                        ..Default::default()
                    },
                );

                if let Some(matrix) = UiState::show_transform_window(ctx, transform_window, Some(text)) {
                    if let Some(id) = selected_id {
                        // even though the header version of this actually modifies the mesh, this just modifies the transform matrix
                        // ideally this would allow it to be undoable, but that's still really complicated...
                        pub fn transform_subobj(model: &mut Model, id: ObjectId, matrix: &TMat4<f32>, transform_offset: bool) {
                            let no_trans_matrix = glm::set_row(matrix, 3, &glm::vec4(0.0, 0.0, 0.0, 1.0));
                            let rot_matrix = no_trans_matrix.normalize();

                            if !transform_offset {
                                model.subobject_transform_matrix[id] *= matrix;
                            } else {
                                model.subobject_transform_matrix[id] *= no_trans_matrix;
                            }

                            for turret in &mut model.turrets {
                                if id == turret.base_obj {
                                    for firepoint in &mut turret.fire_points {
                                        *firepoint = &no_trans_matrix * *firepoint;
                                    }
                                    turret.normal.0 = &rot_matrix * turret.normal.0;
                                }
                            }

                            if let Some((mut uvec, mut fvec)) = model.sub_objects[id].uvec_fvec() {
                                uvec = &rot_matrix * uvec;
                                fvec = &rot_matrix * fvec;
                                pof::properties_update_field(&mut model.sub_objects[id].properties, "$uvec", &uvec.to_string());
                                pof::properties_update_field(&mut model.sub_objects[id].properties, "$fvec", &fvec.to_string());
                            }

                            let children: Vec<_> = model.sub_objects[id].children().copied().collect();
                            for child_id in children {
                                transform_subobj(model, child_id, &no_trans_matrix, true)
                            }
                        }

                        transform_subobj(&mut self.model, id, &matrix, false);

                        transform_window.open = false;
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
                    let id = selected_id.unwrap();
                    let mut new_parent = if new_parent != 0 {
                        self.model.get_obj_id_by_name(&subobj_names_list[new_parent])
                    } else {
                        None
                    };

                    model_action(
                        undo_history,
                        &mut self.model,
                        undo_func(move |model| {
                            let old_parent = model.sub_objects[id].parent;
                            model.make_orphan(id);
                            if let Some(parent_id) = new_parent {
                                new_parent = old_parent; // swap in the old value
                                model.make_parent(parent_id, id);
                            }
                        }),
                    );
                }

                // Properties edit ================================================================

                ui.label("Properties:");
                if let Some(id) = selected_id {
                    if self.model.sub_objects[id].uvec_fvec().is_some() {
                        self.ui_state.display_uvec_fvec = true;
                    }

                    let widget_response = text_edit_multi(
                        ui,
                        format!("{} properties", current_tree_selection),
                        2,
                        &mut self.model,
                        path_func(move |model| &mut model.sub_objects[id].properties),
                        undo_history,
                    );
                    if widget_response.changed() {
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
                            let id = selected_id.unwrap();
                            let mut new_axis = *rot_axis;
                            model_action(
                                undo_history,
                                &mut self.model,
                                undo_func(move |model: &mut Model| {
                                    let obj = &mut model.sub_objects[id];
                                    swap(&mut obj.rotation_axis, &mut new_axis);
                                    if obj.rotation_axis == SubsysRotationAxis::None {
                                        obj.rotation_type = SubsysRotationType::None
                                    } else if obj.rotation_type == SubsysRotationType::None {
                                        obj.rotation_type = SubsysRotationType::Regular
                                    }
                                }),
                            );
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
                            let id = selected_id.unwrap();
                            let mut new_axis = *trans_axis;
                            model_action(
                                undo_history,
                                &mut self.model,
                                undo_func(move |model: &mut Model| {
                                    let obj = &mut model.sub_objects[id];
                                    swap(&mut obj.translation_axis, &mut new_axis);
                                    if obj.translation_axis == SubsysTranslationAxis::None {
                                        obj.translation_type = SubsysTranslationType::None
                                    } else if obj.translation_type == SubsysTranslationType::None {
                                        obj.translation_type = SubsysTranslationType::Regular
                                    }
                                }),
                            );
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

                let tex = if let TreeValue::Textures(TextureTreeValue::Texture(tex)) = current_tree_selection {
                    Some(path_func(move |model| &mut model.textures[tex.0 as usize]))
                } else {
                    None
                };

                ui.label("Texture Name:");
                if model_value_widget!(format!("{} texture name", current_tree_selection), ui, false, tex, texture_name).changed()
                    && self.model.untextured_idx.is_some()
                    && current_tree_selection == TreeValue::Textures(TextureTreeValue::Texture(self.model.untextured_idx.unwrap()))
                {
                    self.model.untextured_idx = None;
                    self.model.recheck_warnings(One(Warning::UntexturedPolygons));
                }

                ui.add_space(5.0);
                if self
                    .model
                    .untextured_idx
                    .map_or(false, |idx| TreeValue::Textures(TextureTreeValue::Texture(idx)) == current_tree_selection)
                {
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

                let (bank_num, point_num) = match current_tree_selection {
                    TreeValue::Thrusters(ThrusterTreeValue::Bank(bank)) => (Some(bank), None),
                    TreeValue::Thrusters(ThrusterTreeValue::BankPoint(bank, point)) => (Some(bank), Some(point)),
                    _ => (None, None),
                };

                let bank_idx_response = UiState::list_manipulator_widget(ui, bank_num, Some(&self.model.thruster_banks), "Bank");

                ui.add_space(10.0);

                ui.horizontal(|ui| {
                    ui.label("Engine Subsystem:");
                    if let Some(bank) = bank_num {
                        if self.model.warnings.contains(&Warning::ThrusterPropertiesInvalidVersion(bank)) {
                            UiState::set_widget_color(ui, WARNING_YELLOW);
                        }

                        if text_edit_single_no_undo(ui, "thrusters engine subsys", engine_subsys_string).changed() {
                            let mut engine_subsys_string = engine_subsys_string.clone();

                            model_action(
                                undo_history,
                                &mut self.model,
                                undo_func(move |model: &mut Model| {
                                    let temp = pof::properties_get_field(&model.thruster_banks[bank].properties, "$engine_subsystem")
                                        .map(|s| s.to_string())
                                        .unwrap_or_default();
                                    pof::properties_update_field(
                                        &mut model.thruster_banks[bank].properties,
                                        "$engine_subsystem",
                                        &engine_subsys_string,
                                    );
                                    engine_subsys_string = temp;
                                }),
                            );
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

                        let widget_response = text_edit_multi(
                            ui,
                            format!("{} properties", current_tree_selection),
                            1,
                            &mut self.model,
                            path_func(move |model| &mut model.thruster_banks[bank].properties),
                            undo_history,
                        );
                        if widget_response.changed() {
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
                    UiState::list_manipulator_widget(ui, point_num, bank_num.map(|bank| &self.model.thruster_banks[bank].glows), "Point");

                ui.add_space(10.0);

                let (pos, norm, radius) = if let TreeValue::Thrusters(ThrusterTreeValue::BankPoint(bank, point)) = current_tree_selection {
                    (
                        Some(path_func(move |model| &mut model.thruster_banks[bank].glows[point].position)),
                        Some(path_func(move |model| &mut model.thruster_banks[bank].glows[point].normal)),
                        Some(path_func(move |model| &mut model.thruster_banks[bank].glows[point].radius)),
                    )
                } else {
                    (None, None, None)
                };

                ui.label("Radius:");
                model_value_widget!(format!("{} radius", current_tree_selection), ui, false, radius, radius_string);
                ui.label("Position:");
                model_value_widget!(format!("{} position", current_tree_selection), ui, false, pos, position_string);
                ui.label("Normal:");
                model_value_widget!(format!("{} normal", current_tree_selection), ui, false, norm, normal_string);

                if let Some(mut response) = bank_idx_response {
                    let new_idx = response.get_new_ui_idx(&self.model.thruster_banks);

                    if response.undoable() {
                        model_action(
                            undo_history,
                            &mut self.model,
                            undo_func(move |model: &mut Model| {
                                response.apply(&mut model.thruster_banks);
                            }),
                        );
                    } else {
                        response.apply(&mut self.model.thruster_banks);
                    }

                    self.model.recheck_warnings(All); //FIX

                    select_new_tree_val!(TreeValue::Thrusters(ThrusterTreeValue::bank(new_idx)));
                } else if let Some(mut response) = point_idx_response {
                    let new_idx = response.get_new_ui_idx(&self.model.thruster_banks[bank_num.unwrap()].glows);

                    if response.undoable() {
                        model_action(
                            undo_history,
                            &mut self.model,
                            undo_func(move |model: &mut Model| {
                                response.apply(&mut model.thruster_banks[bank_num.unwrap()].glows);
                            }),
                        );
                    } else {
                        response.apply(&mut self.model.thruster_banks[bank_num.unwrap()].glows);
                    }

                    select_new_tree_val!(TreeValue::Thrusters(ThrusterTreeValue::bank_point(bank_num.unwrap(), new_idx)));
                }
            }
            PropertiesPanel::Weapon { position_string, normal_string, offset_string } => {
                let weapon_tree_selection = match current_tree_selection {
                    TreeValue::Weapons(select) => select,
                    _ => unreachable!(),
                };
                let (bank_num, point_num) = match weapon_tree_selection {
                    WeaponTreeValue::Header => {
                        ui.heading("Weapons");
                        (None, None)
                    }
                    WeaponTreeValue::PriHeader => {
                        ui.heading("Primary Weapons");
                        (None, None)
                    }
                    WeaponTreeValue::PriBank(bank) => {
                        ui.heading("Primary Weapons");
                        (Some(bank), None)
                    }
                    WeaponTreeValue::PriBankPoint(bank, point) => {
                        ui.heading("Primary Weapons");
                        (Some(bank), Some(point))
                    }
                    WeaponTreeValue::SecHeader => {
                        ui.heading("Secondary Weapons");
                        (None, None)
                    }
                    WeaponTreeValue::SecBank(bank) => {
                        ui.heading("Secondary Weapons");
                        (Some(bank), None)
                    }
                    WeaponTreeValue::SecBankPoint(bank, point) => {
                        ui.heading("Secondary Weapons");
                        (Some(bank), Some(point))
                    }
                };
                let weapon_selection = if let TreeValue::Weapons(selection) = current_tree_selection {
                    selection
                } else {
                    unreachable!()
                };

                ui.separator();

                let weapon_system = weapon_selection.current_weapons_vec(&mut self.model);
                let bank_idx_response = UiState::list_manipulator_widget(ui, bank_num, weapon_system.as_deref(), "Bank");

                ui.add_space(10.0);

                ui.separator();

                let point_idx_response = UiState::list_manipulator_widget(
                    ui,
                    point_num,
                    weapon_system.as_ref().and_then(|weps| bank_num.map(|bank| &weps[bank])),
                    "Point",
                );

                ui.add_space(10.0);

                let (pos, norm, offset) = if let TreeValue::Weapons(WeaponTreeValue::PriBankPoint(bank, point)) = current_tree_selection {
                    (
                        Some(path_func(move |model| &mut model.primary_weps[bank][point].position)),
                        Some(path_func(move |model| &mut model.primary_weps[bank][point].normal)),
                        Some(path_func(move |model| &mut model.primary_weps[bank][point].offset)),
                    )
                } else if let TreeValue::Weapons(WeaponTreeValue::SecBankPoint(bank, point)) = current_tree_selection {
                    (
                        Some(path_func(move |model| &mut model.secondary_weps[bank][point].position)),
                        Some(path_func(move |model| &mut model.secondary_weps[bank][point].normal)),
                        Some(path_func(move |model| &mut model.secondary_weps[bank][point].offset)),
                    )
                } else {
                    (None, None, None)
                };

                ui.label("Position:");
                model_value_widget!(format!("{} position", current_tree_selection), ui, false, pos, position_string);
                ui.label("Normal:");
                model_value_widget!(format!("{} normal", current_tree_selection), ui, false, norm, normal_string);
                ui.label("Offset:");
                let warning = self.model.pof_model.warnings.contains(&Warning::WeaponOffsetInvalidVersion {
                    primary: weapon_selection.is_primary(),
                    bank: bank_num.unwrap_or_default(),
                    point: point_num.unwrap_or_default(),
                });
                let offset_changed =
                    model_value_widget!(format!("{} angle offset", current_tree_selection), ui, warning, offset, offset_string).changed();

                let is_primary = weapon_selection.is_primary();
                if let Some(mut response) = bank_idx_response {
                    let weapon_system = weapon_selection.current_weapons_vec(&mut self.model).unwrap();
                    let new_idx = response.get_new_ui_idx(weapon_system);

                    match (is_primary, response.undoable()) {
                        (true, true) => model_action(
                            undo_history,
                            &mut self.model,
                            undo_func(move |model: &mut Model| {
                                response.apply(&mut model.primary_weps);
                            }),
                        ),
                        (false, true) => model_action(
                            undo_history,
                            &mut self.model,
                            undo_func(move |model: &mut Model| {
                                response.apply(&mut model.secondary_weps);
                            }),
                        ),
                        (true, false) => {
                            response.apply(&mut self.model.primary_weps);
                        }
                        (false, false) => {
                            response.apply(&mut self.model.primary_weps);
                        }
                    }

                    self.model.recheck_warnings(All); // FIX

                    select_new_tree_val!(TreeValue::Weapons(WeaponTreeValue::bank(is_primary, new_idx)));
                } else if let Some(mut response) = point_idx_response {
                    let weapon_system = weapon_selection.current_weapons_vec(&mut self.model).unwrap();
                    let new_idx = response.get_new_ui_idx(&weapon_system[bank_num.unwrap()]);

                    match (is_primary, response.undoable()) {
                        (true, true) => model_action(
                            undo_history,
                            &mut self.model,
                            undo_func(move |model: &mut Model| {
                                response.apply(&mut model.primary_weps[bank_num.unwrap()]);
                            }),
                        ),
                        (false, true) => model_action(
                            undo_history,
                            &mut self.model,
                            undo_func(move |model: &mut Model| {
                                response.apply(&mut model.secondary_weps[bank_num.unwrap()]);
                            }),
                        ),
                        (true, false) => {
                            response.apply(&mut self.model.primary_weps[bank_num.unwrap()]);
                        }
                        (false, false) => {
                            response.apply(&mut self.model.primary_weps[bank_num.unwrap()]);
                        }
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

                let bay_num = match current_tree_selection {
                    TreeValue::DockingBays(DockingTreeValue::Bay(bay)) => Some(bay),
                    _ => None,
                };

                let bay_idx_response = UiState::list_manipulator_widget(ui, bay_num, Some(&self.model.docking_bays), "Bay");

                ui.add_space(10.0);

                ui.horizontal(|ui| {
                    ui.label("Name:");
                    if let Some(bay) = bay_num {
                        if text_edit_single_no_undo(ui, "docking bay name", name_string).changed() {
                            let mut name_string = name_string.clone();

                            model_action(
                                undo_history,
                                &mut self.model,
                                undo_func(move |model: &mut Model| {
                                    let temp = pof::properties_get_field(&model.docking_bays[bay].properties, "$name")
                                        .map(|s| s.to_string())
                                        .unwrap_or_default();
                                    pof::properties_update_field(&mut model.docking_bays[bay].properties, "$name", &name_string);
                                    name_string = temp;
                                }),
                            );
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

                subobj_names_list.extend(self.model.get_subobj_names());

                let mut parent_id = if let Some(bay) = bay_num {
                    self.model.docking_bays[bay].get_parent_obj().map_or(0, |parent_name| {
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
                    let bay_num = bay_num.unwrap();
                    let mut new_name = subobj_names_list[new_subobj].clone();
                    let mut old_name = pof::properties_get_field(&self.model.docking_bays[bay_num].properties, "$parent_submodel")
                        .unwrap_or_default()
                        .to_owned();

                    model_action(
                        undo_history,
                        &mut self.model,
                        undo_func(move |model| {
                            pof::properties_update_field(&mut model.docking_bays[bay_num].properties, "$parent_submodel", &new_name);
                            swap(&mut old_name, &mut new_name);
                        }),
                    );
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
                ui.add_enabled_ui(bay_num.is_some(), |ui| {
                    if egui::ComboBox::from_label("Path")
                        .show_index(ui, path_num, paths.len(), |i| paths[i].to_owned())
                        .changed()
                    {
                        let bay_num = bay_num.unwrap();
                        let mut new_path = if *path_num == self.model.paths.len() {
                            None
                        } else {
                            Some(PathId(*path_num as u32))
                        };

                        model_action(
                            undo_history,
                            &mut self.model,
                            undo_func(move |model| {
                                swap(&mut model.docking_bays[bay_num].path, &mut new_path);
                            }),
                        );
                    }
                });

                ui.separator();

                CollapsingHeader::new("Properties Raw").show(ui, |ui| {
                    if let Some(bay) = bay_num {
                        let widget_response = text_edit_multi(
                            ui,
                            format!("{} properties", current_tree_selection),
                            1,
                            &mut self.model,
                            path_func(move |model| &mut model.docking_bays[bay].properties),
                            undo_history,
                        );
                        if widget_response.changed() {
                            if let Some(new_name) = pof::properties_get_field(&self.model.docking_bays[bay].properties, "$name") {
                                *name_string = new_name.to_string();
                            }
                            self.model.recheck_warnings(One(Warning::DockingBayNameTooLong(bay)));
                            self.model.recheck_warnings(One(Warning::DockingBayPropertiesTooLong(bay)));
                            self.model.recheck_warnings(One(Warning::InvalidDockParentSubmodel(bay)));
                        }
                    } else {
                        ui.add_enabled(false, egui::TextEdit::multiline(&mut String::new()).desired_rows(1));
                    }
                });

                ui.separator();

                ui.add_space(10.0);

                ui.label("Position:");
                let pos = bay_num.map(|num| path_func(move |model| &mut model.docking_bays[num].position));
                model_value_widget!(format!("{} position", current_tree_selection), ui, false, pos, position_string).changed();

                ui.label(RichText::new("Forward Vector:").color(Color32::from_rgb(140, 150, 210)));
                let norm = bay_num.map(|num| {
                    parse_func(move |model: &Model, mut new_val: NormalVec3| {
                        // after getting the new fvec, orthonormalize and get a new uvec too
                        let mut uvec = Dock::orthonormalize(&model.docking_bays[num].uvec.0.into(), &new_val.0.into());
                        undo_func(move |model| {
                            // and use both when applying the undo function
                            swap(&mut new_val, &mut model.docking_bays[num].fvec);
                            swap(&mut uvec, &mut model.docking_bays[num].uvec);
                            info!("Modifying: DockingBays - {} fvec", num);
                        })
                    })
                });
                UiState::model_value_edit_custom(
                    "docking bay fvec",
                    &mut self.ui_state.viewport_3d_dirty,
                    ui,
                    false,
                    norm,
                    fvec_string,
                    undo_history,
                    &mut self.model,
                );

                ui.label(RichText::new("Up Vector:").color(Color32::from_rgb(210, 140, 140)));
                let log_text = format!("{} uvec", current_tree_selection);
                ui.add_enabled_ui(bay_num.is_some(), |ui| {
                    // this undo sucks, it triggers for every single change, fix this
                    if ui.add(DragValue::new(uvec_ang).speed(0.5)).changed() {
                        self.ui_state.viewport_3d_dirty = true;
                        *uvec_ang %= 360.0;
                        let mut uvec_ang = uvec_ang.to_radians();
                        model_action(
                            undo_history,
                            &mut self.model,
                            undo_func(move |model| {
                                let temp = model.docking_bays[bay_num.unwrap()].get_uvec_angle() % TAU;
                                model.docking_bays[bay_num.unwrap()].set_uvec_angle(uvec_ang);
                                uvec_ang = temp;
                                info!("Modifying: {}", log_text);
                            }),
                        )
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

                ui.image(&self.dock_demo_img);

                if let Some(mut response) = bay_idx_response {
                    let new_idx = response.get_new_ui_idx(&self.model.docking_bays);

                    if response.undoable() {
                        model_action(
                            undo_history,
                            &mut self.model,
                            undo_func(move |model: &mut Model| {
                                response.apply(&mut model.docking_bays);
                            }),
                        );
                    } else {
                        response.apply(&mut self.model.docking_bays);
                    }

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

                let (bank_num, point_num) = match current_tree_selection {
                    TreeValue::Glows(GlowTreeValue::Bank(bank)) => (Some(bank), None),
                    TreeValue::Glows(GlowTreeValue::BankPoint(bank, point)) => (Some(bank), Some(point)),
                    _ => (None, None),
                };

                // no subobjects = no glow banks allowed
                let glow_banks_opt = (!self.model.sub_objects.is_empty()).then(|| &self.model.glow_banks);
                let bank_idx_response = UiState::list_manipulator_widget(ui, bank_num, glow_banks_opt, "Bank");

                ui.add_space(10.0);

                ui.label("Glow Texture:");
                if let Some(bank) = bank_num {
                    if text_edit_single_no_undo(ui, "glows tex name", glow_texture_string).changed() {
                        let mut glow_texture_string = glow_texture_string.clone();
                        model_action(
                            undo_history,
                            &mut self.model,
                            undo_func(move |model: &mut Model| {
                                let temp = pof::properties_get_field(&model.glow_banks[bank].properties, "$glow_texture")
                                    .map(|s| s.to_string())
                                    .unwrap_or_default();
                                pof::properties_update_field(&mut model.glow_banks[bank].properties, "$glow_texture", &glow_texture_string);
                                glow_texture_string = temp;
                            }),
                        );
                    }
                } else {
                    ui.add_enabled(false, egui::TextEdit::singleline(&mut blank_string).desired_rows(1));
                }

                let subobj_names_list = self.model.get_subobj_names();

                if let Some(new_subobj) = UiState::subobject_combo_box(ui, &subobj_names_list, attached_subobj_idx, bank_num, "SubObject", None, None)
                {
                    let num = bank_num.unwrap();
                    let mut new_id = ObjectId(new_subobj as u32);
                    model_action(
                        undo_history,
                        &mut self.model,
                        undo_func(move |model| {
                            swap(&mut model.glow_banks[num].obj_parent, &mut new_id);
                        }),
                    );
                }

                let (disp_time, on_time, off_time, lod, glow_type) = if let TreeValue::Glows(GlowTreeValue::BankPoint(bank, _))
                | TreeValue::Glows(GlowTreeValue::Bank(bank)) = current_tree_selection
                {
                    (
                        Some(path_func(move |model| &mut model.glow_banks[bank].disp_time)),
                        Some(path_func(move |model| &mut model.glow_banks[bank].on_time)),
                        Some(path_func(move |model| &mut model.glow_banks[bank].off_time)),
                        Some(path_func(move |model| &mut model.glow_banks[bank].lod)),
                        Some(path_func(move |model| &mut model.glow_banks[bank].glow_type)),
                    )
                } else {
                    (None, None, None, None, None)
                };

                ui.horizontal(|ui| {
                    ui.label("LOD:");
                    model_value_widget!(format!("{} lod", current_tree_selection), ui, false, lod, lod_string);
                });

                ui.horizontal(|ui| {
                    ui.label("Type:");
                    model_value_widget!(format!("{} type", current_tree_selection), ui, false, glow_type, glow_type_string);
                });

                ui.separator();

                if ui.checkbox(&mut self.glow_point_simulation, "Glow Point Simulation").clicked() {
                    self.glow_point_sim_start = std::time::Instant::now();
                    self.ui_state.viewport_3d_dirty = true; // for the case when this is disabled
                }

                ui.label("Displacement Time:");
                model_value_widget!(format!("{} disp time", current_tree_selection), ui, false, disp_time, disp_time_string);

                ui.horizontal(|ui| {
                    ui.label("On Time:");
                    model_value_widget!(format!("{} on time", current_tree_selection), ui, false, on_time, on_time_string);
                });

                ui.horizontal(|ui| {
                    ui.label("Off Time:");
                    model_value_widget!(format!("{} off time", current_tree_selection), ui, false, off_time, off_time_string);
                });

                ui.separator();

                CollapsingHeader::new("Properties Raw").show(ui, |ui| {
                    if let Some(bank) = bank_num {
                        let widget_response = text_edit_multi(
                            ui,
                            format!("{} properties", current_tree_selection),
                            1,
                            &mut self.model,
                            path_func(move |model| &mut model.glow_banks[bank].properties),
                            undo_history,
                        );
                        if widget_response.changed() {
                            self.model.recheck_warnings(One(Warning::GlowBankPropertiesTooLong(bank)));
                        }
                    } else {
                        ui.add_enabled(false, egui::TextEdit::multiline(&mut String::new()).desired_rows(1));
                    }
                });

                ui.separator();

                let glow_points = if let TreeValue::Glows(GlowTreeValue::BankPoint(bank, _)) = current_tree_selection {
                    Some(&mut self.model.glow_banks[bank].glow_points)
                } else if let TreeValue::Glows(GlowTreeValue::Bank(bank)) = current_tree_selection {
                    Some(&mut self.model.glow_banks[bank].glow_points)
                } else {
                    None
                };

                let point_idx_response = UiState::list_manipulator_widget(ui, point_num, glow_points.as_deref(), "Point");

                let (pos, norm, radius) = if let TreeValue::Glows(GlowTreeValue::BankPoint(bank, point)) = current_tree_selection {
                    (
                        Some(path_func(move |model| &mut model.glow_banks[bank].glow_points[point].position)),
                        Some(path_func(move |model| &mut model.glow_banks[bank].glow_points[point].normal)),
                        Some(path_func(move |model| &mut model.glow_banks[bank].glow_points[point].radius)),
                    )
                } else {
                    (None, None, None)
                };

                ui.add_space(10.0);

                ui.label("Radius:");
                model_value_widget!(format!("{} radius", current_tree_selection), ui, false, radius, radius_string);
                ui.label("Position:");
                model_value_widget!(format!("{} position", current_tree_selection), ui, false, pos, position_string);
                ui.label("Normal:");
                model_value_widget!(format!("{} normal", current_tree_selection), ui, false, norm, normal_string);

                if let Some(mut response) = bank_idx_response {
                    let new_idx = response.get_new_ui_idx(&self.model.glow_banks);

                    if response.undoable() {
                        model_action(
                            undo_history,
                            &mut self.model,
                            undo_func(move |model: &mut Model| {
                                response.apply(&mut model.glow_banks);
                            }),
                        );
                    } else {
                        response.apply(&mut self.model.glow_banks);
                    }

                    select_new_tree_val!(TreeValue::Glows(GlowTreeValue::bank(new_idx)));
                } else if let Some(mut response) = point_idx_response {
                    let new_idx = response.get_new_ui_idx(&self.model.glow_banks[bank_num.unwrap()].glow_points);

                    if response.undoable() {
                        model_action(
                            undo_history,
                            &mut self.model,
                            undo_func(move |model: &mut Model| {
                                response.apply(&mut model.glow_banks[bank_num.unwrap()].glow_points);
                            }),
                        );
                    } else {
                        response.apply(&mut self.model.glow_banks[bank_num.unwrap()].glow_points);
                    }

                    select_new_tree_val!(TreeValue::Glows(GlowTreeValue::bank_point(bank_num.unwrap(), new_idx)));
                }
            }
            PropertiesPanel::SpecialPoint { radius_string, position_string, name_string } => {
                ui.heading("Special Point");
                ui.separator();

                let (point_num, name) = match current_tree_selection {
                    TreeValue::SpecialPoints(SpecialPointTreeValue::Point(point)) => {
                        (Some(point), Some(path_func(move |model| &mut model.special_points[point].name)))
                    }
                    _ => (None, None),
                };

                let spec_point_idx_response = UiState::list_manipulator_widget(ui, point_num, Some(self.model.special_points.as_ref()), "Point");

                ui.add_space(10.0);

                ui.horizontal(|ui| {
                    ui.label("Name:");
                    if model_value_widget!(format!("{} name", current_tree_selection), ui, false, name, name_string).changed() {
                        self.model.recheck_warnings(One(Warning::SpecialPointNameTooLong(point_num.unwrap())));
                    }
                });

                ui.separator();

                let types_display = ["", "Subsystem", "Shield point"];
                let types = ["", "subsystem", "shieldpoint"];
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
                            let mut new_val = types[idx].to_string();
                            model_action(
                                undo_history,
                                &mut self.model,
                                undo_func(move |model: &mut Model| {
                                    let val = pof::properties_get_field(&model.special_points[point].properties, "$special")
                                        .unwrap_or_default()
                                        .to_string();
                                    pof::properties_update_field(&mut model.special_points[point].properties, "$special", &new_val);
                                    new_val = val;
                                }),
                            );
                        }
                    } else {
                        egui::ComboBox::from_label("Type").show_ui(ui, |ui| {
                            ui.selectable_value(&mut idx, 0, "");
                        });
                    }
                });

                CollapsingHeader::new("Properties Raw").show(ui, |ui| {
                    if let Some(point) = point_num {
                        let widget_response = text_edit_multi(
                            ui,
                            format!("{} properties", current_tree_selection),
                            1,
                            &mut self.model,
                            path_func(move |model| &mut model.special_points[point].properties),
                            undo_history,
                        );
                        if widget_response.changed() {
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

                let (pos, radius) = if let TreeValue::SpecialPoints(SpecialPointTreeValue::Point(point)) = current_tree_selection {
                    (
                        Some(path_func(move |model| &mut model.special_points[point].position)),
                        Some(path_func(move |model| &mut model.special_points[point].radius)),
                    )
                } else {
                    (None, None)
                };

                ui.label("Radius:");
                model_value_widget!(format!("{} radius", current_tree_selection), ui, false, radius, radius_string);
                ui.label("Position:");
                model_value_widget!(format!("{} position", current_tree_selection), ui, false, pos, position_string);

                if let Some(mut response) = spec_point_idx_response {
                    let new_idx = response.get_new_ui_idx(&self.model.special_points);

                    if response.undoable() {
                        model_action(
                            undo_history,
                            &mut self.model,
                            undo_func(move |model: &mut Model| {
                                response.apply(&mut model.special_points);
                            }),
                        );
                    } else {
                        response.apply(&mut self.model.special_points);
                    }

                    select_new_tree_val!(TreeValue::SpecialPoints(SpecialPointTreeValue::point(new_idx)));
                }
            }
            PropertiesPanel::Turret { position_string, normal_string, base_idx } => {
                ui.heading("Turret");
                ui.separator();

                let (turret_num, point_num) = match current_tree_selection {
                    TreeValue::Turrets(TurretTreeValue::Turret(turret)) => (Some(turret), None),
                    TreeValue::Turrets(TurretTreeValue::TurretPoint(turret, point)) => (Some(turret), Some(point)),
                    _ => (None, None),
                };

                // no subobjects = no turrets allowed
                let turrets_opt = (!self.model.sub_objects.is_empty()).then(|| &self.model.turrets);
                let turret_idx_response = UiState::list_manipulator_widget(ui, turret_num, turrets_opt, "Turret");

                ui.add_space(10.0);
                let subobj_names_list = self.model.get_subobj_names();

                if let Some(new_subobj) = UiState::subobject_combo_box(ui, &subobj_names_list, base_idx, turret_num, "Base object", None, None) {
                    let mut new_val = ObjectId(new_subobj as u32);
                    let turret_num = turret_num.unwrap(); // the unwrap is ok here, if it were none, the combo box would be un-interactable

                    model_action(
                        undo_history,
                        &mut self.model,
                        undo_func(move |model: &mut Model| {
                            let val = &mut model.turrets[turret_num].base_obj;
                            swap(&mut new_val, val);
                        }),
                    );
                }

                // turret gun subobject combo box is a bit trickier since we only want to show valid subobjects (and the currently used one,
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
                    let mut new_val = gun_subobj_ids_list[new_idx];
                    let turret_num = turret_num.unwrap(); // the unwrap is ok here, if it were none, the combo box would be un-interactable

                    model_action(
                        undo_history,
                        &mut self.model,
                        undo_func(move |model: &mut Model| {
                            let val = &mut model.turrets[turret_num].gun_obj;
                            swap(&mut new_val, val);
                        }),
                    );
                }

                let norm =
                    if let TreeValue::Turrets(TurretTreeValue::Turret(turret) | TurretTreeValue::TurretPoint(turret, _)) = current_tree_selection {
                        Some(path_func(move |model| &mut model.turrets[turret].normal))
                    } else {
                        None
                    };

                ui.label("Normal:");
                model_value_widget!(format!("{} normal", current_tree_selection), ui, false, norm, normal_string);

                ui.separator();
                ui.add(Label::new(RichText::new("Turret Fire Points").text_style(TextStyle::Button)));
                ui.separator();

                let point_idx_response =
                    UiState::list_manipulator_widget(ui, point_num, turret_num.map(|num| &self.model.turrets[num].fire_points), "Fire Point");

                ui.add_space(10.0);

                let pos = if let TreeValue::Turrets(TurretTreeValue::TurretPoint(turret, point)) = current_tree_selection {
                    Some(path_func(move |model| &mut model.turrets[turret].fire_points[point]))
                } else {
                    None
                };

                ui.label("Position:");
                model_value_widget!(format!("{} position", current_tree_selection), ui, false, pos, position_string);

                if let Some(mut response) = turret_idx_response {
                    let new_idx = response.get_new_ui_idx(&self.model.turrets);

                    if response.undoable() {
                        model_action(
                            undo_history,
                            &mut self.model,
                            undo_func(move |model: &mut Model| {
                                response.apply(&mut model.turrets);
                            }),
                        );
                    } else {
                        response.apply(&mut self.model.turrets);
                    }

                    select_new_tree_val!(TreeValue::Turrets(TurretTreeValue::turret(new_idx)));
                } else if let Some(mut response) = point_idx_response {
                    let new_idx = response.get_new_ui_idx(&self.model.turrets[turret_num.unwrap()].fire_points);

                    if response.undoable() {
                        model_action(
                            undo_history,
                            &mut self.model,
                            undo_func(move |model: &mut Model| {
                                response.apply(&mut model.turrets[turret_num.unwrap()].fire_points);
                            }),
                        );
                    } else {
                        response.apply(&mut self.model.turrets[turret_num.unwrap()].fire_points);
                    }

                    select_new_tree_val!(TreeValue::Turrets(TurretTreeValue::turret_point(turret_num.unwrap(), new_idx)));
                }
            }
            PropertiesPanel::Path { name_string, parent_string, position_string, radius_string } => {
                ui.heading("Path");
                ui.separator();

                let (path_num, point_num) = match current_tree_selection {
                    TreeValue::Paths(PathTreeValue::Path(path)) => (Some(path), None),
                    TreeValue::Paths(PathTreeValue::PathPoint(path, point)) => (Some(path), Some(point)),
                    _ => (None, None),
                };

                let path_idx_response = UiState::list_manipulator_widget(ui, path_num, Some(&self.model.paths), "Path");

                ui.add_space(10.0);

                let (name, parent) = match current_tree_selection {
                    TreeValue::Paths(PathTreeValue::PathPoint(path, _)) | TreeValue::Paths(PathTreeValue::Path(path)) => {
                        (Some(path_func(move |model| &mut model.paths[path].name)), Some(path_func(move |model| &mut model.paths[path].parent)))
                    }
                    _ => (None, None),
                };

                ui.label("Name:");
                if let Some(num) = path_num {
                    let old_name = self.model.paths[num].name.clone();
                    let warnings = self.model.warnings.contains(&Warning::DuplicatePathName(old_name.clone()))
                        || self.model.warnings.contains(&Warning::PathNameTooLong(num));

                    if model_value_widget!(format!("{} name", current_tree_selection), ui, warnings, name, name_string).changed() {
                        self.model.recheck_warnings(One(Warning::DuplicatePathName(old_name)));
                        let new_name = self.model.pof_model.paths[num].name.clone();
                        self.model.recheck_warnings(One(Warning::DuplicatePathName(new_name)));
                        self.model.recheck_warnings(One(Warning::PathNameTooLong(num)));
                    }
                } else {
                    ui.add_enabled(false, egui::TextEdit::multiline(name_string).desired_rows(1));
                }

                ui.label("Parent:");
                model_value_widget!(format!("{} parent", current_tree_selection), ui, false, parent, parent_string);

                ui.separator();

                let point_idx_response = UiState::list_manipulator_widget(ui, point_num, path_num.map(|num| &self.model.paths[num].points), "Point");

                ui.add_space(10.0);

                let (radius, pos) = if let TreeValue::Paths(PathTreeValue::PathPoint(path, point)) = current_tree_selection {
                    (
                        Some(path_func(move |model| &mut model.paths[path].points[point].radius)),
                        Some(path_func(move |model| &mut model.paths[path].points[point].position)),
                    )
                } else {
                    (None, None)
                };

                ui.label("Radius:");
                model_value_widget!(format!("{} radius", current_tree_selection), ui, false, radius, radius_string);
                ui.label("Position:");
                model_value_widget!(format!("{} position", current_tree_selection), ui, false, pos, position_string);

                if let Some(mut response) = path_idx_response {
                    let new_idx = response.get_new_ui_idx(&self.model.paths);

                    if response.undoable() {
                        // a path deletion or insertion requires any docking bays connected to that path be updated
                        // as well as any other path references be re-indexed
                        let mut relevant_dock_bays = vec![];
                        let modified_path_id = if let IndexingButtonsResponse::Delete(idx) = response {
                            PathId(idx as u32)
                        } else {
                            PathId(new_idx.unwrap() as u32)
                        };

                        if let IndexingButtonsResponse::Delete(_) = response {
                            relevant_dock_bays = self
                                .model
                                .docking_bays
                                .iter()
                                .enumerate()
                                .filter_map(|(bay_idx, bay)| if bay.path == Some(modified_path_id) { Some(bay_idx) } else { None })
                                .collect::<Vec<_>>();
                        }

                        model_action(
                            undo_history,
                            &mut self.model,
                            undo_func(move |model: &mut Model| {
                                if let IndexingButtonsResponse::Delete(_) = response {
                                    relevant_dock_bays = model
                                        .docking_bays
                                        .iter()
                                        .enumerate()
                                        .filter_map(|(bay_idx, bay)| if bay.path == Some(modified_path_id) { Some(bay_idx) } else { None })
                                        .collect::<Vec<_>>();

                                    model.path_removal_fixup(modified_path_id);
                                    for bay_idx in &relevant_dock_bays {
                                        model.docking_bays[*bay_idx].path = None;
                                    }
                                } else if let IndexingButtonsResponse::Insert(idx, _) = response {
                                    model.path_insertion_fixup(idx);
                                    for bay_idx in &relevant_dock_bays {
                                        model.docking_bays[*bay_idx].path = Some(PathId(idx as u32));
                                    }
                                }
                                response.apply(&mut model.paths);
                            }),
                        );
                    } else {
                        response.apply(&mut self.model.paths);
                    }

                    self.model.recheck_warnings(All); // FIX

                    select_new_tree_val!(TreeValue::Paths(PathTreeValue::path(new_idx)));
                } else if let Some(mut response) = point_idx_response {
                    let new_idx = response.get_new_ui_idx(&self.model.paths[path_num.unwrap()].points);

                    if response.undoable() {
                        model_action(
                            undo_history,
                            &mut self.model,
                            undo_func(move |model: &mut Model| {
                                response.apply(&mut model.paths[path_num.unwrap()].points);
                            }),
                        );
                    } else {
                        response.apply(&mut self.model.paths[path_num.unwrap()].points);
                    }

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

                let (lod, offset) = if let TreeValue::Insignia(InsigniaTreeValue::Insignia(idx)) = current_tree_selection {
                    (
                        Some(path_func(move |model| &mut model.insignias[idx].detail_level)),
                        Some(path_func(move |model| &mut model.insignias[idx].offset)),
                    )
                } else {
                    (None, None)
                };

                ui.label("Detail Level:");
                model_value_widget!(format!("{} detail level", current_tree_selection), ui, false, lod, lod_string);
                ui.label("Offset:");
                model_value_widget!(format!("{} offset", current_tree_selection), ui, false, offset, offset_string);
            }
            PropertiesPanel::EyePoint { position_string, normal_string, attached_subobj_idx } => {
                ui.heading("Eye Point");
                ui.separator();

                let eye_num = match current_tree_selection {
                    TreeValue::EyePoints(EyeTreeValue::EyePoint(point)) => Some(point),
                    _ => None,
                };

                // no subobjects = no eye points allowed
                let eye_points_opt = (!self.model.sub_objects.is_empty()).then(|| &self.model.eye_points);
                let eye_idx_response = UiState::list_manipulator_widget(ui, eye_num, eye_points_opt, "Eye Point");

                ui.add_space(10.0);

                ui.add_enabled_ui(eye_num.is_some(), |ui| {
                    if let Some(num) = eye_num {
                        let mut name_list = self.model.get_subobj_names();
                        if self.model.eye_points[num].attached_subobj.is_none() {
                            name_list.push("None".to_string());
                        }

                        let changed = egui::ComboBox::from_label("Attached submodel")
                            .show_index(ui, attached_subobj_idx, name_list.len(), |i| name_list[i].to_owned())
                            .changed();

                        if changed && *attached_subobj_idx < self.model.sub_objects.len() {
                            let mut new_val = Some(ObjectId(*attached_subobj_idx as u32));
                            model_action(
                                undo_history,
                                &mut self.model,
                                undo_func(move |model: &mut Model| {
                                    let val = &mut model.eye_points[num].attached_subobj;
                                    swap(&mut new_val, val);
                                }),
                            );
                        }
                    } else {
                        egui::ComboBox::from_label("Attached submodel").show_index(ui, attached_subobj_idx, 1, |_| format!(""));
                    }
                });

                ui.separator();

                ui.add_space(10.0);

                let (pos, norm) = if let TreeValue::EyePoints(EyeTreeValue::EyePoint(point)) = current_tree_selection {
                    (
                        Some(path_func(move |model| &mut model.eye_points[point].position)),
                        Some(path_func(move |model| &mut model.eye_points[point].normal)),
                    )
                } else {
                    (None, None)
                };
                ui.label("Position:");
                model_value_widget!(format!("{} position", current_tree_selection), ui, false, pos, position_string);
                ui.label("Normal:");
                model_value_widget!(format!("{} normal", current_tree_selection), ui, false, norm, normal_string);

                if let Some(mut response) = eye_idx_response {
                    let new_idx = response.get_new_ui_idx(&self.model.eye_points);

                    if response.undoable() {
                        model_action(
                            undo_history,
                            &mut self.model,
                            undo_func(move |model: &mut Model| {
                                response.apply(&mut model.eye_points);
                            }),
                        );
                    } else {
                        response.apply(&mut self.model.eye_points);
                    }

                    select_new_tree_val!(TreeValue::EyePoints(EyeTreeValue::point(new_idx)));
                }
            }
            PropertiesPanel::VisualCenter { position } => {
                ui.heading("Visual Center");
                ui.separator();

                ui.label("The visual center is treated as the center for things like the targeting box, or tech room.");

                model_value_widget!(
                    format!("{} position", current_tree_selection),
                    ui,
                    false,
                    Some(path_func(move |model| &mut model.visual_center)),
                    position
                );

                ui.add_space(5.0);

                if ui
                    .button("Recalculate")
                    .on_hover_text("Chooses the average position of its surface area")
                    .clicked()
                {
                    let (_, mut new_center) = self.model.surface_area_average_pos();
                    model_action(
                        undo_history,
                        &mut self.model,
                        undo_func(move |model: &mut Model| {
                            let val = &mut model.visual_center;
                            info!("Recalculating: VisualCenter position");
                            swap(&mut new_center, val);
                        }),
                    );
                }
            }
            PropertiesPanel::Comments => {
                ui.heading("Comments");
                ui.separator();
                text_edit_multi(
                    ui,
                    format!(
                        "Model
                        Comments"
                    ),
                    3,
                    &mut self.model,
                    path_func(move |model| &mut model.comments),
                    undo_history,
                );
            }
        }

        if merge_duplicate_textures {
            use pof::TextureId;
            let mut tex_name_map = HashMap::new();
            let mut changed_id_map = HashMap::new();
            let mut new_textures = vec![];
            for (i, tex) in self.model.textures.iter().enumerate() {
                if tex_name_map.contains_key(tex) {
                    changed_id_map.insert(TextureId(i as u32), tex_name_map[tex]);
                } else {
                    changed_id_map.insert(TextureId(i as u32), TextureId(tex_name_map.len() as u32));
                    tex_name_map.insert(tex, TextureId(tex_name_map.len() as u32));
                    new_textures.push(tex.clone());
                }
            }

            let mut new_map = self.model.texture_map.clone();
            for (id1, id2) in changed_id_map {
                *(new_map.get_mut(&id1).unwrap()) = id2;
            }

            model_action(
                undo_history,
                &mut self.model,
                undo_func(move |model: &mut Model| {
                    swap(&mut new_map, &mut model.texture_map);
                    swap(&mut new_textures, &mut model.textures);
                }),
            );

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
