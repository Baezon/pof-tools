use egui::{CollapsingHeader, Label};
use pof::{
    DockingPoint, EyePoint, GlowPoint, GlowPointBank, Insignia, Model, PathPoint, SpecialPoint, SubObject, SubsysMovementAxis, TextureId,
    ThrusterGlow, Turret, WeaponHardpoint,
};
use std::{collections::BTreeSet, str::FromStr, sync::mpsc::Receiver};

use eframe::egui::{self, Button, TextStyle, Ui};
use pof::ObjectId;

use crate::{GlBufferedInsignia, GlBufferedObject, GlBufferedShield, GlLollipops};

enum PropertiesPanel {
    Header {
        bbox_min_string: String,
        bbox_max_string: String,
        radius_string: String,
        mass_string: String,
        moir_string: String,
        moiu_string: String,
        moif_string: String,
    },
    SubObject {
        bbox_min_string: String,
        bbox_max_string: String,
        name: String,
        offset_string: String,
        radius_string: String,
        is_debris_check: bool,
        properties: String,
        rot_axis: SubsysMovementAxis,
    },
    Texture {
        texture_name: String,
    },
    Thruster {
        normal_string: String,
        position_string: String,
        radius_string: String,
        properties: String,
    },
    Weapon {
        position_string: String,
        normal_string: String,
        offset_string: String,
    },
    DockingBay {
        position_string: String,
        normal_string: String,
        properties: String,
    },
    GlowBank {
        disp_time_string: String,
        on_time_string: String,
        off_time_string: String,
        attached_subobj_idx: usize,
        lod_string: String,
        glow_type_string: String,
        properties_string: String,
        position_string: String,
        normal_string: String,
        radius_string: String,
    },
    SpecialPoint {
        name_string: String,
        position_string: String,
        radius_string: String,
        properties: String,
    },
    Turret {
        base_idx: usize,
        gun_idx: usize,
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
    AutoCenter {
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
            properties: Default::default(),
            rot_axis: SubsysMovementAxis::NONE,
        }
    }
    fn default_texture() -> Self {
        Self::Texture { texture_name: Default::default() }
    }
    fn default_thruster() -> Self {
        Self::Thruster {
            radius_string: Default::default(),
            normal_string: Default::default(),
            position_string: Default::default(),
            properties: Default::default(),
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
            position_string: Default::default(),
            normal_string: Default::default(),
            properties: Default::default(),
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
            properties_string: Default::default(),
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
            properties: Default::default(),
        }
    }
    fn default_turret() -> Self {
        Self::Turret {
            base_idx: Default::default(),
            gun_idx: Default::default(),
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

#[derive(PartialEq, Hash)]
pub(crate) enum TreeSelection {
    Header,
    SubObjects(SubObjectSelection),
    Textures(TextureSelection),
    Weapons(WeaponSelection),
    DockingBays(DockingSelection),
    Thrusters(ThrusterSelection),
    Glows(GlowSelection),
    SpecialPoints(SpecialPointSelection),
    Turrets(TurretSelection),
    Paths(PathSelection),
    Shield,
    EyePoints(EyeSelection),
    Insignia(InsigniaSelection),
    AutoCenter,
    Comments,
}
impl Default for TreeSelection {
    fn default() -> Self {
        Self::Header
    }
}
impl TreeSelection {
    // fn name<'a>(&self, model: &'a Model) -> &'a str {
    //     match self {
    //         TreeSelection::Header => "Header",
    //         TreeSelection::SubObjects(_) => todo!(),
    //         TreeSelection::Textures(tex) => match tex {
    //             TextureSelection::Header => "Textures",
    //             TextureSelection::Texture(tex) => &model.textures[tex.0 as usize],
    //         },
    //         TreeSelection::Weapons(_) => todo!(),
    //         TreeSelection::DockingBays(_) => todo!(),
    //         TreeSelection::Thrusters(_) => todo!(),
    //         TreeSelection::Glows(_) => todo!(),
    //         TreeSelection::SpecialPoints(_) => todo!(),
    //         TreeSelection::Turrets(_) => todo!(),
    //         TreeSelection::Paths(_) => todo!(),
    //         TreeSelection::Shield => todo!(),
    //         TreeSelection::Insignia(_) => todo!(),
    //         TreeSelection::EyePoints(_) => todo!(),
    //         TreeSelection::AutoCenter => todo!(),
    //         TreeSelection::Comments => todo!(),
    //     }
    // }

    // fn children<R>(&self, model: &Model) {
    //     match self {
    //         TreeSelection::Header => todo!(),
    //         TreeSelection::SubObjects(_) => todo!(),
    //         TreeSelection::Textures(_) => todo!(),
    //         TreeSelection::Weapons(_) => todo!(),
    //         TreeSelection::DockingBays(_) => todo!(),
    //         TreeSelection::Thrusters(_) => todo!(),
    //         TreeSelection::Glows(_) => todo!(),
    //         TreeSelection::SpecialPoints(_) => todo!(),
    //         TreeSelection::Turrets(_) => todo!(),
    //         TreeSelection::Paths(_) => todo!(),
    //         TreeSelection::Shield => todo!(),
    //         TreeSelection::Insignia(_) => todo!(),
    //         TreeSelection::EyePoints(_) => todo!(),
    //         TreeSelection::AutoCenter => todo!(),
    //         TreeSelection::Comments => todo!(),
    //     }
    // }
}
#[derive(PartialEq, Hash)]
pub(crate) enum InsigniaSelection {
    Header,
    Insignia(usize), // insignia idx
}
impl InsigniaSelection {
    fn _insignia(point: Option<usize>) -> Self {
        match point {
            Some(point) => Self::Insignia(point),
            None => Self::Header,
        }
    }
}

#[derive(PartialEq, Hash)]
pub(crate) enum EyeSelection {
    Header,
    EyePoint(usize), // eye idx
}
impl EyeSelection {
    fn point(point: Option<usize>) -> Self {
        match point {
            Some(point) => Self::EyePoint(point),
            None => Self::Header,
        }
    }
}

#[derive(PartialEq, Hash)]
pub(crate) enum PathSelection {
    Header,
    Path(usize),             // path idx
    PathPoint(usize, usize), // path idx, point idx
}
impl PathSelection {
    fn path_point(path: usize, point: Option<usize>) -> Self {
        match point {
            Some(point) => Self::PathPoint(path, point),
            None => Self::Path(path),
        }
    }
    fn path(path: Option<usize>) -> Self {
        match path {
            Some(path) => Self::Path(path),
            None => Self::Header,
        }
    }
}

#[derive(PartialEq, Hash)]
pub(crate) enum TurretSelection {
    Header,
    Turret(usize),             // turret idx
    TurretPoint(usize, usize), // turret idx, point idx
}
impl TurretSelection {
    fn turret_point(turret: usize, point: Option<usize>) -> Self {
        match point {
            Some(point) => Self::TurretPoint(turret, point),
            None => Self::Turret(turret),
        }
    }
    fn turret(turret: Option<usize>) -> Self {
        match turret {
            Some(turret) => Self::Turret(turret),
            None => Self::Header,
        }
    }
}

#[derive(PartialEq, Hash)]
pub(crate) enum SpecialPointSelection {
    Header,
    Point(usize),
}
impl SpecialPointSelection {
    fn point(point: Option<usize>) -> Self {
        match point {
            Some(point) => Self::Point(point),
            None => Self::Header,
        }
    }
}

#[derive(PartialEq, Hash)]
pub(crate) enum GlowSelection {
    Header,
    Bank(usize),             // bank idx
    BankPoint(usize, usize), // bank idx, point idx
}
impl GlowSelection {
    fn bank_point(bank: usize, point: Option<usize>) -> Self {
        match point {
            Some(point) => Self::BankPoint(bank, point),
            None => Self::Bank(bank),
        }
    }
    fn bank(bank: Option<usize>) -> Self {
        match bank {
            Some(bank) => Self::Bank(bank),
            None => Self::Header,
        }
    }
}

#[derive(PartialEq, Hash)]
pub(crate) enum DockingSelection {
    Header,
    Bay(usize),             // bank idx
    BayPoint(usize, usize), // bank idx, point idx
}
impl DockingSelection {
    fn bay_point(bay: usize, point: Option<usize>) -> Self {
        match point {
            Some(point) => Self::BayPoint(bay, point),
            None => Self::Bay(bay),
        }
    }
    fn bay(bay: Option<usize>) -> Self {
        match bay {
            Some(bay) => Self::Bay(bay),
            None => Self::Header,
        }
    }
}

#[derive(PartialEq, Hash)]
pub(crate) enum WeaponSelection {
    Header,
    PriHeader,
    PriBank(usize),             // bank idx
    PriBankPoint(usize, usize), // bank idx, point idx
    SecHeader,
    SecBank(usize),             // bank idx
    SecBankPoint(usize, usize), // bank idx, point idx
}
impl WeaponSelection {
    fn bank_point(is_primary: bool, bank: usize, point: Option<usize>) -> Self {
        match (is_primary, point) {
            (true, Some(point)) => Self::PriBankPoint(bank, point),
            (true, None) => Self::PriBank(bank),
            (false, Some(point)) => Self::SecBankPoint(bank, point),
            (false, None) => Self::SecBank(bank),
        }
    }
    fn bank(is_primary: bool, bank: Option<usize>) -> Self {
        match (is_primary, bank) {
            (true, Some(bank)) => Self::PriBank(bank),
            (true, None) => Self::PriHeader,
            (false, Some(bank)) => Self::SecBank(bank),
            (false, None) => Self::SecHeader,
        }
    }
}

#[derive(PartialEq, Hash)]
pub(crate) enum TextureSelection {
    Header,
    Texture(TextureId),
}

#[derive(PartialEq, Hash)]
pub(crate) enum ThrusterSelection {
    Header,
    Bank(usize),             // bank idx
    BankPoint(usize, usize), // bank idx, point idx
}
impl ThrusterSelection {
    fn bank_point(bank: usize, point: Option<usize>) -> Self {
        match point {
            Some(point) => Self::BankPoint(bank, point),
            None => Self::Bank(bank),
        }
    }
    fn bank(bank: Option<usize>) -> Self {
        match bank {
            Some(bank) => Self::Bank(bank),
            None => Self::Header,
        }
    }
}

#[derive(PartialEq, Hash)]
pub(crate) enum SubObjectSelection {
    Header,
    SubObject(ObjectId),
}

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

use Set::*;

pub enum Set<T> {
    All,
    One(T),
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub enum Warning {
    RadiusTooSmall(Option<ObjectId>),
}

#[derive(Default)]
pub(crate) struct UiState {
    pub tree_view_selection: TreeSelection,
    pub viewport_3d_dirty: bool,
    properties_panel: PropertiesPanel,
    properties_panel_dirty: bool,
}

#[derive(Default)]
pub(crate) struct PofToolsGui {
    pub model: Box<Model>,
    pub ui_state: UiState,
    pub wireframe_enabled: bool,
    pub loading_thread: Option<Receiver<Option<Box<Model>>>>,
    pub warnings: BTreeSet<Warning>,

    pub camera_pitch: f32,
    pub camera_heading: f32,
    pub camera_scale: f32,

    pub buffer_objects: Vec<GlBufferedObject>, // all the subobjects, conditionally rendered based on the current tree selection
    pub buffer_shield: Option<GlBufferedShield>, // the shield, similar to the above
    pub buffer_insignias: Vec<GlBufferedInsignia>, // the insignias, similar to the above
    pub lollipops: Vec<GlLollipops>, // the current set of lollipops being being drawn, grouped by color, and recalculated with viewport_3d_dirty above
}
impl std::ops::Deref for PofToolsGui {
    type Target = UiState;

    fn deref(&self) -> &Self::Target {
        &self.ui_state
    }
}
impl std::ops::DerefMut for PofToolsGui {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.ui_state
    }
}
impl PofToolsGui {
    fn warnings_contains_any_subobj_radius_too_small(&self) -> bool {
        matches!(self.warnings.range(Warning::RadiusTooSmall(Some(ObjectId(0)))..).next(), Some(Warning::RadiusTooSmall(Some(_))))
    }

    fn tree_selectable_item(&mut self, ui: &mut Ui, name: &str, selection: TreeSelection) {
        self.ui_state.tree_selectable_item(&self.model, ui, name, selection);
    }
}

impl UiState {
    fn tree_selectable_item(&mut self, model: &Model, ui: &mut Ui, name: &str, selection: TreeSelection) {
        if ui.selectable_value(&mut self.tree_view_selection, selection, name).clicked() {
            self.refresh_properties_panel(model);
            self.viewport_3d_dirty = true;
        }
    }

    fn tree_collapsing_item(&mut self, model: &Model, ui: &mut Ui, name: &str, tree_value: TreeSelection, f: impl FnOnce(&mut UiState, &mut Ui)) {
        let response = CollapsingHeader::new(name)
            .selectable(true)
            .id_source(&tree_value)
            .selected(tree_value == self.tree_view_selection)
            .show(ui, |ui| f(self, ui));
        if response.header_response.clicked() {
            self.tree_view_selection = tree_value;
            self.refresh_properties_panel(model);
            self.viewport_3d_dirty = true;
        }
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
                    egui::Label::new(current_num.map_or_else(|| format!("-"), |num| format!("{} {}", index_name, num + 1))),
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
                        {
                            if let Some(_) = list_len {
                                ret = Some(IndexingButtonsResponse::Copy(current_num.unwrap()));
                            }
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
                        {
                            if let Some(_) = list_len {
                                ret = Some(IndexingButtonsResponse::Delete(current_num.unwrap()));
                            }
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
                } else {
                    if ui
                        .add_sized([side_button_size, height], egui::Button::new("▶"))
                        .on_hover_text(format!("Switch to the next {}", index_name))
                        .clicked()
                    {
                        ret = Some(IndexingButtonsResponse::Switch(current_num.map_or(0, |num| num + 1)));
                    }
                }
            });
        });
        ret
    }

    // a text edit field attached to a model value that will show up red if it cannot parse
    fn parsable_text_edit<T: FromStr>(ui: &mut Ui, model_value: &mut T, parsable_string: &mut String) -> bool {
        if let Err(_) = parsable_string.parse::<T>() {
            ui.visuals_mut().override_text_color = Some(egui::Color32::RED);
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
    fn subobject_combo_box(
        ui: &mut Ui, name_list: &Vec<String>, mut_selection: &mut usize, selector_value: Option<usize>, label: &str,
    ) -> Option<ObjectId> {
        let mut ret = None;
        ui.add_enabled_ui(selector_value.is_some(), |ui| {
            if let Some(_) = selector_value {
                egui::ComboBox::from_label(label).show_index(ui, mut_selection, name_list.len(), |i| name_list[i].to_owned());
                ret = Some(ObjectId(*mut_selection as u32));
            } else {
                egui::ComboBox::from_label(label).show_index(ui, mut_selection, 1, |_| format!(""));
            }
        });
        ret
    }

    fn model_value_edit<T: FromStr>(
        viewport_3d_dirty: &mut bool, ui: &mut Ui, active_warning: bool, model_value: Option<&mut T>, parsable_string: &mut String,
    ) -> bool {
        let mut val_changed = false;
        if let Some(value) = model_value {
            if let Err(_) = parsable_string.parse::<T>() {
                ui.visuals_mut().override_text_color = Some(egui::Color32::RED);
            } else if active_warning {
                ui.visuals_mut().override_text_color = Some(egui::Color32::YELLOW);
            }
            if ui.text_edit_singleline(parsable_string).changed() {
                if let Ok(parsed_string) = parsable_string.parse() {
                    *value = parsed_string;
                    *viewport_3d_dirty = true;
                    val_changed = true;
                }
            }
            ui.visuals_mut().override_text_color = None;
        } else {
            ui.add_enabled(false, egui::widgets::TextEdit::singleline(parsable_string));
        }
        val_changed
    }

    // fills the properties panel based on the current tree selection, taking all the relevant data from the model
    pub(crate) fn refresh_properties_panel(&mut self, model: &Model) {
        match &self.tree_view_selection {
            TreeSelection::Header => {
                self.properties_panel = PropertiesPanel::Header {
                    bbox_min_string: format!("{}", model.header.bounding_box.min),
                    bbox_max_string: format!("{}", model.header.bounding_box.max),
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
                }
            }
            TreeSelection::SubObjects(subobj_tree_select) => match subobj_tree_select {
                SubObjectSelection::Header => self.properties_panel = PropertiesPanel::default_subobject(),
                SubObjectSelection::SubObject(id) => {
                    self.properties_panel = PropertiesPanel::SubObject {
                        bbox_max_string: format!("{}", model.sub_objects[id.0 as usize].bbox.max),
                        bbox_min_string: format!("{}", model.sub_objects[id.0 as usize].bbox.min),
                        offset_string: format!("{}", model.sub_objects[id.0 as usize].offset),
                        radius_string: format!("{}", model.sub_objects[id.0 as usize].radius),
                        is_debris_check: model.sub_objects[id.0 as usize].is_debris_model,
                        properties: format!("{}", model.sub_objects[id.0 as usize].properties),
                        name: format!("{}", model.sub_objects[id.0 as usize].name),
                        rot_axis: model.sub_objects[id.0 as usize].movement_axis,
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
            TreeSelection::Thrusters(thruster_tree_select) => match *thruster_tree_select {
                ThrusterSelection::Header => self.properties_panel = PropertiesPanel::default_thruster(),
                ThrusterSelection::Bank(bank) => {
                    self.properties_panel = PropertiesPanel::Thruster {
                        radius_string: Default::default(),
                        normal_string: Default::default(),
                        position_string: Default::default(),
                        properties: model.thruster_banks[bank].properties.clone(),
                    }
                }
                ThrusterSelection::BankPoint(bank, point) => {
                    self.properties_panel = PropertiesPanel::Thruster {
                        radius_string: format!("{}", model.thruster_banks[bank].glows[point].radius),
                        normal_string: format!("{}", model.thruster_banks[bank].glows[point].normal),
                        position_string: format!("{}", model.thruster_banks[bank].glows[point].position),
                        properties: model.thruster_banks[bank].properties.clone(),
                    }
                }
            },
            TreeSelection::Weapons(weapons_tree_select) => match *weapons_tree_select {
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
            TreeSelection::DockingBays(docking_select) => match *docking_select {
                DockingSelection::BayPoint(bay, point) => {
                    self.properties_panel = PropertiesPanel::DockingBay {
                        position_string: format!("{}", model.docking_bays[bay].points[point].position),
                        normal_string: format!("{}", model.docking_bays[bay].points[point].normal),
                        properties: format!("{}", model.docking_bays[bay].properties),
                    }
                }
                _ => self.properties_panel = PropertiesPanel::default_docking_bay(),
            },
            TreeSelection::Glows(glow_select) => match *glow_select {
                GlowSelection::BankPoint(bank, point) => {
                    self.properties_panel = PropertiesPanel::GlowBank {
                        disp_time_string: format!("{}", model.glow_banks[bank].disp_time),
                        on_time_string: format!("{}", model.glow_banks[bank].on_time),
                        off_time_string: format!("{}", model.glow_banks[bank].off_time),
                        attached_subobj_idx: model.glow_banks[bank].obj_parent.0 as usize,
                        lod_string: format!("{}", model.glow_banks[bank].lod),
                        glow_type_string: format!("{}", model.glow_banks[bank].glow_type),
                        properties_string: format!("{}", model.glow_banks[bank].properties),
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
                        properties_string: format!("{}", model.glow_banks[bank].properties),
                        position_string: Default::default(),
                        normal_string: Default::default(),
                        radius_string: Default::default(),
                    }
                }
                _ => self.properties_panel = PropertiesPanel::default_glow(),
            },
            TreeSelection::SpecialPoints(special_select) => match *special_select {
                SpecialPointSelection::Point(point) => {
                    self.properties_panel = PropertiesPanel::SpecialPoint {
                        name_string: format!("{}", model.special_points[point].name),
                        position_string: format!("{}", model.special_points[point].position),
                        radius_string: format!("{}", model.special_points[point].radius),
                        properties: format!("{}", model.special_points[point].properties),
                    }
                }
                _ => self.properties_panel = PropertiesPanel::default_special_point(),
            },
            TreeSelection::Turrets(turret_selection) => match *turret_selection {
                TurretSelection::TurretPoint(turret, point) => {
                    self.properties_panel = PropertiesPanel::Turret {
                        normal_string: format!("{}", model.turrets[turret].normal),
                        base_idx: model.turrets[turret].base_obj.0 as usize,
                        gun_idx: model.turrets[turret].gun_obj.0 as usize,
                        position_string: format!("{}", model.turrets[turret].fire_points[point]),
                    }
                }
                TurretSelection::Turret(turret) => {
                    println!("{:#?}", model.turrets[turret]);
                    self.properties_panel = PropertiesPanel::Turret {
                        normal_string: format!("{}", model.turrets[turret].normal),
                        base_idx: model.turrets[turret].base_obj.0 as usize,
                        gun_idx: model.turrets[turret].gun_obj.0 as usize,
                        position_string: Default::default(),
                    }
                }
                _ => self.properties_panel = PropertiesPanel::default_turret(),
            },
            TreeSelection::Paths(path_selection) => match *path_selection {
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
            TreeSelection::Insignia(insig_selection) => match *insig_selection {
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
                        position_string: format!("{}", model.eye_points[*idx].offset),
                        normal_string: format!("{}", model.eye_points[*idx].normal),
                        attached_subobj_idx: model.eye_points[*idx].attached_subobj.0 as usize,
                    }
                }
                _ => self.properties_panel = PropertiesPanel::default_eye(),
            },
            TreeSelection::Shield => self.properties_panel = PropertiesPanel::Shield, // nothing mutable to refresh! woohoo!'
            TreeSelection::AutoCenter => self.properties_panel = PropertiesPanel::AutoCenter { position: format!("{}", model.auto_center) },
            TreeSelection::Comments => self.properties_panel = PropertiesPanel::Comments,
        }
    }
}

impl PofToolsGui {
    // =====================================================
    // The big top-level function for drawing and interacting with all of the UI
    // ====================================================
    pub fn show_ui(&mut self, ctx: &egui::CtxRef) {
        egui::TopBottomPanel::top("menu").default_height(33.0).min_height(33.0).show(ctx, |ui| {
            Ui::add_space(ui, 6.0);
            ui.horizontal(|ui| {
                if ui.add(Button::new("🗁").text_style(TextStyle::Heading)).clicked() {
                    self.start_loading_model();
                    ui.output().cursor_icon = egui::CursorIcon::Wait;
                }

                if ui.add(Button::new("🖴").text_style(TextStyle::Heading)).clicked() {
                    PofToolsGui::save_model(&self.model);
                }

                ui.separator();

                if ui
                    .add(Button::new(if self.wireframe_enabled { "⏹" } else { "⛶" }).text_style(TextStyle::Heading))
                    .clicked()
                {
                    self.wireframe_enabled = !self.wireframe_enabled;
                }
            });
        });
        let mut warnings = egui::TopBottomPanel::bottom("info bar")
            .resizable(true)
            .default_height(16.0)
            .height_range(16.0..=500.0)
            .show(ctx, |ui| {
                egui::ScrollArea::vertical().auto_shrink([false, false]).show(ui, |ui| {
                    for warning in &self.warnings {
                        match warning {
                            Warning::RadiusTooSmall(id_opt) => {
                                let str = format!(
                                    "⚠ {}'s radius does not encompass all of its geometry",
                                    id_opt.map_or("The header", |id| &self.model.sub_objects[id.0 as usize].name)
                                );
                                ui.add(Label::new(str).text_style(TextStyle::Button).text_color(egui::Color32::YELLOW));
                            }
                        }
                    }
                });
            });
        warnings.response.sense.click = true;
        if warnings.response.clicked() {
            println!("clicked!")
        }

        // ==============================================================================================================
        // The 'tree view' is the section on the left of the UI which contains selections for the various kinds of things
        // the user can edit, turrets, hardpoints, subobjects, etc
        // ==============================================================================================================

        egui::SidePanel::left("tree_view")
            .resizable(true)
            .default_width(200.0)
            .width_range(150.0..=500.0)
            .show(ctx, |ui| {
                egui::ScrollArea::vertical().auto_shrink([false, false]).show(ui, |ui| {
                    self.tree_selectable_item(ui, "Header", TreeSelection::Header);

                    self.ui_state.tree_collapsing_item(
                        &self.model,
                        ui,
                        "SubObjects",
                        TreeSelection::SubObjects(SubObjectSelection::Header),
                        |ui_state, ui| {
                            fn make_subobject_child_list(ui_state: &mut UiState, model: &Model, obj: &SubObject, ui: &mut Ui) {
                                let name = format!("{} ({:#?})", obj.name, obj.obj_id);
                                let selection = TreeSelection::SubObjects(SubObjectSelection::SubObject(obj.obj_id));
                                if obj.children.is_empty() {
                                    ui_state.tree_selectable_item(model, ui, &name, selection);
                                } else {
                                    ui_state.tree_collapsing_item(model, ui, &name, selection, |ui_state, ui| {
                                        for i in &obj.children {
                                            make_subobject_child_list(ui_state, model, &model.sub_objects[i.0 as usize], ui)
                                        }
                                    });
                                }
                            }

                            for object in &self.model.sub_objects {
                                if object.parent.is_none() {
                                    make_subobject_child_list(ui_state, &self.model, object, ui);
                                }
                            }
                        },
                    );

                    // self.ui_state.tree_item(&self.model, ui,
                    //     "Textures", TreeSelection::Textures(TextureTreeSelection::Header),
                    //     self.model.textures.iter().enumerate(),
                    //     |ui_state, ui, (i, tex)| {
                    //         ui_state.tree_selectable_item(
                    //             &self.model, ui, tex,
                    //             TreeSelection::Textures(TextureTreeSelection::Texture(TextureId(i as u32))));
                    //     });
                    self.ui_state.tree_collapsing_item(
                        &self.model,
                        ui,
                        "Textures",
                        TreeSelection::Textures(TextureSelection::Header),
                        |ui_state, ui| {
                            for (i, tex) in self.model.textures.iter().enumerate() {
                                ui_state.tree_selectable_item(
                                    &self.model,
                                    ui,
                                    tex,
                                    TreeSelection::Textures(TextureSelection::Texture(TextureId(i as u32))),
                                );
                            }
                        },
                    );

                    // self.ui_state.tree_item(&self.model, ui, "Thrusters",
                    //     TreeSelection::Thrusters(ThrusterTreeSelection::Header),
                    //     self.model.thruster_banks.iter().enumerate(),
                    //     |ui_state, ui, (i, thruster_bank)| {
                    //         ui_state.tree_item(&self.model, ui, &format!("Bank {}", i + 1),
                    //             TreeSelection::Thrusters(ThrusterTreeSelection::ThrusterBank(i)),
                    //             thruster_bank.glows.iter().enumerate(),
                    //             |ui_state, ui, (j, _)| {
                    //                 ui_state.tree_selectable_item(
                    //                     &self.model, ui, &format!("Point {}", j + 1),
                    //                     TreeSelection::Thrusters(ThrusterTreeSelection::ThrusterPoint(i, j)));
                    //             });
                    //     });

                    self.ui_state.tree_collapsing_item(
                        &self.model,
                        ui,
                        "Thrusters",
                        TreeSelection::Thrusters(ThrusterSelection::Header),
                        |ui_state, ui| {
                            for (i, thruster_bank) in self.model.thruster_banks.iter().enumerate() {
                                ui_state.tree_collapsing_item(
                                    &self.model,
                                    ui,
                                    &format!("Bank {}", i + 1),
                                    TreeSelection::Thrusters(ThrusterSelection::Bank(i)),
                                    |ui_state, ui| {
                                        for j in 0..thruster_bank.glows.len() {
                                            ui_state.tree_selectable_item(
                                                &self.model,
                                                ui,
                                                &format!("Point {}", j + 1),
                                                TreeSelection::Thrusters(ThrusterSelection::BankPoint(i, j)),
                                            );
                                        }
                                    },
                                );
                            }
                        },
                    );

                    self.ui_state.tree_collapsing_item(
                        &self.model,
                        ui,
                        "Weapons",
                        TreeSelection::Weapons(WeaponSelection::Header),
                        |ui_state, ui| {
                            ui_state.tree_collapsing_item(
                                &self.model,
                                ui,
                                "Primary Weapons",
                                TreeSelection::Weapons(WeaponSelection::PriHeader),
                                |ui_state, ui| {
                                    for (i, primary_bank) in self.model.primary_weps.iter().enumerate() {
                                        ui_state.tree_collapsing_item(
                                            &self.model,
                                            ui,
                                            &format!("Bank {}", i + 1),
                                            TreeSelection::Weapons(WeaponSelection::PriBank(i)),
                                            |ui_state, ui| {
                                                for j in 0..primary_bank.len() {
                                                    ui_state.tree_selectable_item(
                                                        &self.model,
                                                        ui,
                                                        &format!("Point {}", j + 1),
                                                        TreeSelection::Weapons(WeaponSelection::PriBankPoint(i, j)),
                                                    );
                                                }
                                            },
                                        );
                                    }
                                },
                            );

                            ui_state.tree_collapsing_item(
                                &self.model,
                                ui,
                                "Secondary Weapons",
                                TreeSelection::Weapons(WeaponSelection::SecHeader),
                                |ui_state, ui| {
                                    for (i, secondary_bank) in self.model.secondary_weps.iter().enumerate() {
                                        ui_state.tree_collapsing_item(
                                            &self.model,
                                            ui,
                                            &format!("Bank {}", i + 1),
                                            TreeSelection::Weapons(WeaponSelection::SecBank(i)),
                                            |ui_state, ui| {
                                                for j in 0..secondary_bank.len() {
                                                    ui_state.tree_selectable_item(
                                                        &self.model,
                                                        ui,
                                                        &format!("Point {}", j + 1),
                                                        TreeSelection::Weapons(WeaponSelection::SecBankPoint(i, j)),
                                                    );
                                                }
                                            },
                                        );
                                    }
                                },
                            );
                        },
                    );

                    self.ui_state.tree_collapsing_item(
                        &self.model,
                        ui,
                        "Docking Bays",
                        TreeSelection::DockingBays(DockingSelection::Header),
                        |ui_state, ui| {
                            for (i, docking_bay) in self.model.docking_bays.iter().enumerate() {
                                ui_state.tree_collapsing_item(
                                    &self.model,
                                    ui,
                                    &format!("Bay {}", i + 1),
                                    TreeSelection::DockingBays(DockingSelection::Bay(i)),
                                    |ui_state, ui| {
                                        for j in 0..docking_bay.points.len() {
                                            ui_state.tree_selectable_item(
                                                &self.model,
                                                ui,
                                                &format!("Point {}", j + 1),
                                                TreeSelection::DockingBays(DockingSelection::BayPoint(i, j)),
                                            );
                                        }
                                    },
                                );
                            }
                        },
                    );

                    self.ui_state.tree_collapsing_item(
                        &self.model,
                        ui,
                        "Glow Points",
                        TreeSelection::Glows(GlowSelection::Header),
                        |ui_state, ui| {
                            for (i, glow_bank) in self.model.glow_banks.iter().enumerate() {
                                ui_state.tree_collapsing_item(
                                    &self.model,
                                    ui,
                                    &format!("Bank {}", i + 1),
                                    TreeSelection::Glows(GlowSelection::Bank(i)),
                                    |ui_state, ui| {
                                        for j in 0..glow_bank.glow_points.len() {
                                            ui_state.tree_selectable_item(
                                                &self.model,
                                                ui,
                                                &format!("Point {}", j + 1),
                                                TreeSelection::Glows(GlowSelection::BankPoint(i, j)),
                                            );
                                        }
                                    },
                                );
                            }
                        },
                    );

                    self.ui_state.tree_collapsing_item(
                        &self.model,
                        ui,
                        "Special Points",
                        TreeSelection::SpecialPoints(SpecialPointSelection::Header),
                        |ui_state, ui| {
                            for (i, special_point) in self.model.special_points.iter().enumerate() {
                                ui_state.tree_selectable_item(
                                    &self.model,
                                    ui,
                                    &special_point.name,
                                    TreeSelection::SpecialPoints(SpecialPointSelection::Point(i)),
                                );
                            }
                        },
                    );

                    self.ui_state.tree_collapsing_item(
                        &self.model,
                        ui,
                        "Turrets",
                        TreeSelection::Turrets(TurretSelection::Header),
                        |ui_state, ui| {
                            for (i, turret) in self.model.turrets.iter().enumerate() {
                                ui_state.tree_collapsing_item(
                                    &self.model,
                                    ui,
                                    &self.model.sub_objects[turret.base_obj.0 as usize].name,
                                    TreeSelection::Turrets(TurretSelection::Turret(i)),
                                    |ui_state, ui| {
                                        for j in 0..turret.fire_points.len() {
                                            ui_state.tree_selectable_item(
                                                &self.model,
                                                ui,
                                                &format!("Fire Point {}", j + 1),
                                                TreeSelection::Turrets(TurretSelection::TurretPoint(i, j)),
                                            );
                                        }
                                    },
                                );
                            }
                        },
                    );

                    self.ui_state
                        .tree_collapsing_item(&self.model, ui, "Paths", TreeSelection::Paths(PathSelection::Header), |ui_state, ui| {
                            for (i, path) in self.model.paths.iter().enumerate() {
                                ui_state.tree_collapsing_item(
                                    &self.model,
                                    ui,
                                    &path.name,
                                    TreeSelection::Paths(PathSelection::Path(i)),
                                    |ui_state, ui| {
                                        for j in 0..path.points.len() {
                                            ui_state.tree_selectable_item(
                                                &self.model,
                                                ui,
                                                &format!("Path Point {}", j + 1),
                                                TreeSelection::Paths(PathSelection::PathPoint(i, j)),
                                            );
                                        }
                                    },
                                );
                            }
                        });

                    self.ui_state.tree_collapsing_item(
                        &self.model,
                        ui,
                        "Insignias",
                        TreeSelection::Insignia(InsigniaSelection::Header),
                        |ui_state, ui| {
                            for (i, _) in self.model.insignias.iter().enumerate() {
                                ui_state.tree_selectable_item(
                                    &self.model,
                                    ui,
                                    &format!("Insignia {}", i + 1),
                                    TreeSelection::Insignia(InsigniaSelection::Insignia(i)),
                                );
                            }
                        },
                    );

                    self.ui_state.tree_selectable_item(&self.model, ui, "Shield", TreeSelection::Shield);

                    self.ui_state.tree_collapsing_item(
                        &self.model,
                        ui,
                        "Eye Points",
                        TreeSelection::EyePoints(EyeSelection::Header),
                        |ui_state, ui| {
                            for (i, eye) in self.model.eye_points.iter().enumerate() {
                                ui_state.tree_selectable_item(
                                    &self.model,
                                    ui,
                                    &format!("{} {}", self.model.sub_objects[eye.attached_subobj.0 as usize].name, i + 1),
                                    TreeSelection::EyePoints(EyeSelection::EyePoint(i)),
                                );
                            }
                        },
                    );

                    self.ui_state
                        .tree_selectable_item(&self.model, ui, "Auto-Center", TreeSelection::AutoCenter);

                    self.ui_state.tree_selectable_item(&self.model, ui, "Comments", TreeSelection::Comments);
                });
            });

        // ==============================================================================================================
        // The 'properties panel' is the section on the right of the UI which contains the specific fields ready for
        // manipulation of whatever it is they have selected in the tree view
        // ==============================================================================================================

        egui::SidePanel::right("properties_panel")
            .resizable(true)
            .default_width(200.0)
            .width_range(200.0..=500.0)
            .show(ctx, |ui| {
                egui::ScrollArea::vertical().auto_shrink([false, false]).show(ui, |ui| {
                    match &mut self.ui_state.properties_panel {
                        PropertiesPanel::Header {
                            bbox_min_string,
                            bbox_max_string,
                            mass_string,
                            radius_string,
                            moir_string,
                            moiu_string,
                            moif_string,
                        } => {
                            ui.heading("Header");
                            ui.separator();

                            let (bbox_min, bbox_max, radius, mass, moi_r, moi_u, moi_f) = {
                                (
                                    Some(&mut self.model.header.bounding_box.min),
                                    Some(&mut self.model.header.bounding_box.max),
                                    Some(&mut self.model.header.max_radius),
                                    Some(&mut self.model.header.mass),
                                    Some(&mut self.model.header.moment_of_inertia.rvec),
                                    Some(&mut self.model.header.moment_of_inertia.uvec),
                                    Some(&mut self.model.header.moment_of_inertia.fvec),
                                )
                            };

                            ui.label("Bounding Box Min:");
                            UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, bbox_min, bbox_min_string);
                            ui.label("Bounding Box Max:");
                            UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, bbox_max, bbox_max_string);
                            ui.label("Radius:");
                            let radius_changed = UiState::model_value_edit(
                                &mut self.ui_state.viewport_3d_dirty,
                                ui,
                                self.warnings.contains(&Warning::RadiusTooSmall(None)),
                                radius,
                                radius_string,
                            );
                            ui.label("Mass:");
                            UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, mass, mass_string);
                            ui.label("Moment of Intertia:");
                            UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, moi_r, moir_string);
                            UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, moi_u, moiu_string);
                            UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, moi_f, moif_string);

                            if radius_changed {
                                PofToolsGui::recheck_warnings(&mut self.warnings, &self.model, One(Warning::RadiusTooSmall(None)));
                            }
                        }
                        PropertiesPanel::SubObject {
                            bbox_min_string,
                            bbox_max_string,
                            name,
                            offset_string,
                            radius_string,
                            is_debris_check,
                            properties,
                            rot_axis,
                        } => {
                            ui.heading("SubObject");
                            ui.separator();

                            let selected_id = if let TreeSelection::SubObjects(SubObjectSelection::SubObject(id)) = self.ui_state.tree_view_selection
                            {
                                Some(id)
                            } else {
                                None
                            };

                            ui.label("Name:");
                            if let Some(id) = selected_id {
                                ui.add(egui::TextEdit::singleline(&mut self.model.sub_objects[id.0 as usize].name));
                            } else {
                                ui.add_enabled(false, egui::TextEdit::singleline(name));
                            }

                            ui.add_space(5.0);

                            ui.add_enabled_ui(selected_id.is_some(), |ui| {
                                if ui.checkbox(is_debris_check, "Debris Subobject").changed() {
                                    self.model.sub_objects[selected_id.unwrap().0 as usize].is_debris_model = *is_debris_check;
                                }
                            });

                            ui.add_space(5.0);

                            let (bbox_min, bbox_max, offset, radius) =
                                if let TreeSelection::SubObjects(SubObjectSelection::SubObject(id)) = self.ui_state.tree_view_selection {
                                    let SubObject { bbox, offset, radius, .. } = &mut self.model.sub_objects[id.0 as usize];
                                    (Some(&mut bbox.min), Some(&mut bbox.max), Some(offset), Some(radius))
                                } else {
                                    (None, None, None, None)
                                };

                            ui.label("Bounding Box Min:");
                            UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, bbox_min, bbox_min_string);
                            ui.label("Bounding Box Max:");
                            UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, bbox_max, bbox_max_string);
                            ui.label("Offset:");
                            UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, offset, offset_string);
                            ui.label("Radius:");
                            if UiState::model_value_edit(
                                &mut self.ui_state.viewport_3d_dirty,
                                ui,
                                self.warnings.contains(&Warning::RadiusTooSmall(selected_id)),
                                radius,
                                radius_string,
                            ) {
                                PofToolsGui::recheck_warnings(&mut self.warnings, &self.model, One(Warning::RadiusTooSmall(selected_id)));
                            }

                            ui.label("Properties:");
                            if let Some(id) = selected_id {
                                ui.add(egui::TextEdit::multiline(&mut self.model.sub_objects[id.0 as usize].properties).desired_rows(2));
                            } else {
                                ui.add_enabled(false, egui::TextEdit::multiline(properties).desired_rows(2));
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
                                self.model.sub_objects[selected_id.unwrap().0 as usize].movement_axis = *rot_axis;
                            }
                        }
                        PropertiesPanel::Texture { texture_name } => {
                            ui.heading("Textures");
                            ui.separator();

                            let tex = if let TreeSelection::Textures(TextureSelection::Texture(tex)) = self.ui_state.tree_view_selection {
                                Some(&mut self.model.textures[tex.0 as usize])
                            } else {
                                None
                            };

                            ui.label("Texture Name:");
                            UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, tex, texture_name);
                        }
                        PropertiesPanel::Thruster { position_string, normal_string, radius_string, properties } => {
                            ui.heading("Thruster");
                            ui.separator();

                            let (bank_num, point_num) = match self.ui_state.tree_view_selection {
                                TreeSelection::Thrusters(ThrusterSelection::Bank(bank)) => (Some(bank), None),
                                TreeSelection::Thrusters(ThrusterSelection::BankPoint(bank, point)) => (Some(bank), Some(point)),
                                _ => (None, None),
                            };

                            let bank_idx_response = UiState::list_manipulator_widget(ui, bank_num, Some(self.model.thruster_banks.len()), "Bank");

                            ui.add_space(10.0);

                            ui.label("Properties:");
                            if let Some(bank) = bank_num {
                                ui.add(egui::TextEdit::multiline(&mut self.model.thruster_banks[bank].properties).desired_rows(1));
                            } else {
                                ui.add_enabled(false, egui::TextEdit::multiline(properties).desired_rows(1));
                            }

                            ui.separator();

                            let point_idx_response = UiState::list_manipulator_widget(
                                ui,
                                point_num,
                                bank_num.map(|bank| self.model.thruster_banks[bank].glows.len()),
                                "Point",
                            );

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
                                self.ui_state.properties_panel_dirty = true;
                                self.ui_state.viewport_3d_dirty = true;
                            } else if let Some(response) = point_idx_response {
                                let new_idx = response.apply(&mut self.model.thruster_banks[bank_num.unwrap()].glows);

                                self.ui_state.tree_view_selection =
                                    TreeSelection::Thrusters(ThrusterSelection::bank_point(bank_num.unwrap(), new_idx));
                                self.ui_state.properties_panel_dirty = true;
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
                            ui.separator();

                            let bank_idx_response =
                                UiState::list_manipulator_widget(ui, bank_num, weapon_system.as_ref().map(|weps| weps.0.len()), "Bank");

                            ui.add_space(10.0);

                            ui.separator();

                            let point_idx_response = UiState::list_manipulator_widget(
                                ui,
                                point_num,
                                weapon_system.as_ref().and_then(|weps| bank_num.map(|bank| weps.0[bank].len())),
                                "Point",
                            );

                            ui.add_space(10.0);

                            let (pos, norm, offset) = if let TreeSelection::Weapons(
                                WeaponSelection::PriBankPoint(bank, point) | WeaponSelection::SecBankPoint(bank, point),
                            ) = self.ui_state.tree_view_selection
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
                            UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, offset, offset_string);

                            if let Some(response) = bank_idx_response {
                                let (weapon_system, is_primary) = weapon_system.unwrap();
                                let new_idx = response.apply(weapon_system);

                                self.ui_state.tree_view_selection = TreeSelection::Weapons(WeaponSelection::bank(is_primary, new_idx));
                                self.ui_state.properties_panel_dirty = true;
                                self.ui_state.viewport_3d_dirty = true;
                            } else if let Some(response) = point_idx_response {
                                let (weapon_system, is_primary) = weapon_system.unwrap();
                                let new_idx = response.apply(&mut weapon_system[bank_num.unwrap()]);

                                self.ui_state.tree_view_selection =
                                    TreeSelection::Weapons(WeaponSelection::bank_point(is_primary, bank_num.unwrap(), new_idx));
                                self.ui_state.properties_panel_dirty = true;
                                self.ui_state.viewport_3d_dirty = true;
                            }
                        }
                        PropertiesPanel::DockingBay { position_string, normal_string, properties } => {
                            ui.heading("Docking Bay");
                            ui.separator();

                            let (bay_num, point_num) = match self.ui_state.tree_view_selection {
                                TreeSelection::DockingBays(DockingSelection::Bay(bay)) => (Some(bay), None),
                                TreeSelection::DockingBays(DockingSelection::BayPoint(bay, point)) => (Some(bay), Some(point)),
                                _ => (None, None),
                            };

                            let bay_idx_response = UiState::list_manipulator_widget(ui, bay_num, Some(self.model.docking_bays.len()), "Bay");

                            ui.add_space(10.0);

                            ui.label("Properties:");
                            if let Some(bay) = bay_num {
                                ui.add(egui::TextEdit::multiline(&mut self.model.docking_bays[bay].properties).desired_rows(1));
                            } else {
                                ui.add_enabled(false, egui::TextEdit::multiline(properties).desired_rows(1));
                            }

                            ui.separator();

                            let point_idx_response = UiState::list_manipulator_widget(
                                ui,
                                point_num,
                                bay_num.map(|bay| self.model.docking_bays[bay].points.len()),
                                "Point",
                            );

                            ui.add_space(10.0);

                            let (pos, norm) =
                                if let TreeSelection::DockingBays(DockingSelection::BayPoint(bay, point)) = self.ui_state.tree_view_selection {
                                    let DockingPoint { position, normal } = &mut self.model.docking_bays[bay].points[point];
                                    (Some(position), Some(normal))
                                } else {
                                    (None, None)
                                };
                            ui.label("Position:");
                            UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, pos, position_string);
                            ui.label("Normal:");
                            UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, norm, normal_string);

                            if let Some(response) = bay_idx_response {
                                let new_idx = response.apply(&mut self.model.docking_bays);

                                self.ui_state.tree_view_selection = TreeSelection::DockingBays(DockingSelection::bay(new_idx));
                                self.ui_state.properties_panel_dirty = true;
                                self.ui_state.viewport_3d_dirty = true;
                            } else if let Some(response) = point_idx_response {
                                let new_idx = response.apply(&mut self.model.docking_bays[bay_num.unwrap()].points);

                                self.ui_state.tree_view_selection =
                                    TreeSelection::DockingBays(DockingSelection::bay_point(bay_num.unwrap(), new_idx));
                                self.ui_state.properties_panel_dirty = true;
                                self.ui_state.viewport_3d_dirty = true;
                            }
                        }
                        PropertiesPanel::GlowBank {
                            disp_time_string,
                            on_time_string,
                            off_time_string,
                            lod_string,
                            glow_type_string,
                            properties_string,
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

                            let bank_idx_response = UiState::list_manipulator_widget(ui, bank_num, Some(self.model.glow_banks.len()), "Bank");

                            ui.add_space(10.0);

                            ui.label("Properties:");
                            if let Some(bank) = bank_num {
                                ui.add(egui::TextEdit::multiline(&mut self.model.glow_banks[bank].properties).desired_rows(1));
                            } else {
                                ui.add_enabled(false, egui::TextEdit::multiline(properties_string).desired_rows(1));
                            }

                            let subobj_names_list = self.model.get_subobj_names();

                            let (disp_time, on_time, off_time, lod, glow_type, attached_subobj, glow_points) =
                                if let TreeSelection::Glows(GlowSelection::BankPoint(bank, _)) = self.ui_state.tree_view_selection {
                                    let GlowPointBank {
                                        disp_time,
                                        on_time,
                                        off_time,
                                        lod,
                                        glow_type,
                                        obj_parent,
                                        glow_points,
                                        ..
                                    } = &mut self.model.glow_banks[bank as usize];

                                    (Some(disp_time), Some(on_time), Some(off_time), Some(lod), Some(glow_type), Some(obj_parent), Some(glow_points))
                                } else if let TreeSelection::Glows(GlowSelection::Bank(bank)) = self.ui_state.tree_view_selection {
                                    let GlowPointBank {
                                        disp_time,
                                        on_time,
                                        off_time,
                                        lod,
                                        glow_type,
                                        obj_parent,
                                        glow_points,
                                        ..
                                    } = &mut self.model.glow_banks[bank as usize];

                                    (Some(disp_time), Some(on_time), Some(off_time), Some(lod), Some(glow_type), Some(obj_parent), Some(glow_points))
                                } else {
                                    (None, None, None, None, None, None, None)
                                };

                            ui.horizontal(|ui| {
                                ui.label("LOD:");
                                UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, lod, lod_string);
                            });

                            ui.horizontal(|ui| {
                                ui.label("Type:");
                                UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, glow_type, glow_type_string);
                            });

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

                            if let Some(new_subobj) = UiState::subobject_combo_box(ui, &subobj_names_list, attached_subobj_idx, bank_num, "SubObject")
                            {
                                *attached_subobj.unwrap() = new_subobj;
                            }

                            ui.separator();

                            let point_idx_response = UiState::list_manipulator_widget(ui, point_num, glow_points.map(|list| list.len()), "Point");

                            let (pos, norm, radius) =
                                if let TreeSelection::Glows(GlowSelection::BankPoint(bank, point)) = self.ui_state.tree_view_selection {
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
                                self.ui_state.properties_panel_dirty = true;
                                self.ui_state.viewport_3d_dirty = true;
                            } else if let Some(response) = point_idx_response {
                                let new_idx = response.apply(&mut self.model.glow_banks[bank_num.unwrap()].glow_points);

                                self.ui_state.tree_view_selection = TreeSelection::Glows(GlowSelection::bank_point(bank_num.unwrap(), new_idx));
                                self.ui_state.properties_panel_dirty = true;
                                self.ui_state.viewport_3d_dirty = true;
                            }
                        }
                        PropertiesPanel::SpecialPoint { radius_string, position_string, name_string, properties } => {
                            ui.heading("Special Point");
                            ui.separator();

                            let point_num = match self.ui_state.tree_view_selection {
                                TreeSelection::SpecialPoints(SpecialPointSelection::Point(point)) => Some(point),
                                _ => None,
                            };

                            let spec_point_idx_response =
                                UiState::list_manipulator_widget(ui, point_num, Some(self.model.special_points.len()), "Point");

                            ui.add_space(10.0);

                            ui.label("Name:");
                            if let Some(point) = point_num {
                                ui.add(egui::TextEdit::singleline(&mut self.model.special_points[point].name));
                            } else {
                                ui.add_enabled(false, egui::TextEdit::singleline(name_string));
                            }

                            ui.label("Properties:");
                            if let Some(point) = point_num {
                                ui.add(egui::TextEdit::multiline(&mut self.model.special_points[point].properties).desired_rows(1));
                            } else {
                                ui.add_enabled(false, egui::TextEdit::multiline(properties).desired_rows(1));
                            }

                            ui.separator();

                            ui.add_space(10.0);

                            let (pos, radius) =
                                if let TreeSelection::SpecialPoints(SpecialPointSelection::Point(point)) = self.ui_state.tree_view_selection {
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
                                self.ui_state.properties_panel_dirty = true;
                                self.ui_state.viewport_3d_dirty = true;
                            }
                        }
                        PropertiesPanel::Turret { position_string, normal_string, base_idx, gun_idx } => {
                            ui.heading("Turret");
                            ui.separator();

                            let (turret_num, point_num) = match self.ui_state.tree_view_selection {
                                TreeSelection::Turrets(TurretSelection::Turret(turret)) => (Some(turret), None),
                                TreeSelection::Turrets(TurretSelection::TurretPoint(turret, point)) => (Some(turret), Some(point)),
                                _ => (None, None),
                            };

                            let turret_idx_response = UiState::list_manipulator_widget(ui, turret_num, Some(self.model.turrets.len()), "Turret");

                            ui.add_space(10.0);
                            let subobj_names_list = self.model.get_subobj_names();

                            let (base_obj, gun_obj, norm) =
                                if let TreeSelection::Turrets(TurretSelection::Turret(turret)) = self.ui_state.tree_view_selection {
                                    let Turret { base_obj, gun_obj, normal, .. } = &mut self.model.turrets[turret];
                                    (Some(base_obj), Some(gun_obj), Some(normal))
                                } else if let TreeSelection::Turrets(TurretSelection::TurretPoint(turret, _)) = self.ui_state.tree_view_selection {
                                    let Turret { base_obj, gun_obj, normal, .. } = &mut self.model.turrets[turret];
                                    (Some(base_obj), Some(gun_obj), Some(normal))
                                } else {
                                    (None, None, None)
                                };

                            if let Some(new_subobj) = UiState::subobject_combo_box(ui, &subobj_names_list, base_idx, turret_num, "Base object") {
                                *base_obj.unwrap() = new_subobj;
                            }
                            if let Some(new_subobj) = UiState::subobject_combo_box(ui, &subobj_names_list, gun_idx, turret_num, "Gun object") {
                                *gun_obj.unwrap() = new_subobj;
                            }

                            ui.label("Normal:");
                            UiState::model_value_edit(&mut self.ui_state.viewport_3d_dirty, ui, false, norm, normal_string);

                            ui.separator();
                            ui.add(Label::new("Turret Fire Points").text_style(TextStyle::Button));
                            ui.separator();

                            let point_idx_response = UiState::list_manipulator_widget(
                                ui,
                                point_num,
                                turret_num.map(|num| self.model.turrets[num].fire_points.len()),
                                "Fire Point",
                            );

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

                                self.ui_state.tree_view_selection = TreeSelection::Turrets(TurretSelection::turret(new_idx));
                                self.ui_state.properties_panel_dirty = true;
                                self.ui_state.viewport_3d_dirty = true;
                            } else if let Some(response) = point_idx_response {
                                let new_idx = response.apply(&mut self.model.turrets[turret_num.unwrap()].fire_points);

                                self.ui_state.tree_view_selection =
                                    TreeSelection::Turrets(TurretSelection::turret_point(turret_num.unwrap(), new_idx));
                                self.ui_state.properties_panel_dirty = true;
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

                            let (radius, pos) = if let TreeSelection::Paths(PathSelection::PathPoint(path, point)) = self.ui_state.tree_view_selection
                            {
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
                                let new_idx = response.apply(&mut self.model.paths);

                                self.ui_state.tree_view_selection = TreeSelection::Paths(PathSelection::path(new_idx));
                                self.ui_state.properties_panel_dirty = true;
                                self.ui_state.viewport_3d_dirty = true;
                            } else if let Some(response) = point_idx_response {
                                let new_idx = response.apply(&mut self.model.paths[path_num.unwrap()].points);

                                self.ui_state.tree_view_selection = TreeSelection::Paths(PathSelection::path_point(path_num.unwrap(), new_idx));
                                self.ui_state.properties_panel_dirty = true;
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

                            let eye_idx_response = UiState::list_manipulator_widget(ui, eye_num, Some(self.model.eye_points.len()), "Eye Point");

                            ui.add_space(10.0);

                            ui.add_enabled_ui(eye_num.is_some(), |ui| {
                                if let Some(num) = eye_num {
                                    let name_list = self.model.get_subobj_names();
                                    egui::ComboBox::from_label("Attached submodel").show_index(
                                        ui,
                                        attached_subobj_idx,
                                        self.model.sub_objects.len(),
                                        |i| name_list[i].to_owned(),
                                    );
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
                                self.ui_state.properties_panel_dirty = true;
                                self.ui_state.viewport_3d_dirty = true;
                            }
                        }
                        PropertiesPanel::AutoCenter { position } => {
                            ui.heading("Auto-Center");
                            ui.separator();
                            if UiState::parsable_text_edit(ui, &mut self.model.auto_center, position) {
                                self.viewport_3d_dirty = true;
                            }
                        }
                        PropertiesPanel::Comments => {
                            ui.heading("Comments");
                            ui.separator();
                            ui.text_edit_multiline(&mut self.model.comments);
                        }
                    }

                    if self.properties_panel_dirty {
                        self.ui_state.refresh_properties_panel(&self.model);
                        self.properties_panel_dirty = false;
                    }
                });
            });
    }

    // rechecks just one or all of the warnings on the model
    pub fn recheck_warnings(warnings: &mut BTreeSet<Warning>, model: &Model, warning_to_check: Set<Warning>) {
        if let One(warning) = warning_to_check {
            let failed_check;
            match warning {
                Warning::RadiusTooSmall(subobj_opt) => {
                    failed_check = PofToolsGui::radius_test_failed(model, subobj_opt);
                }
            }

            let existing_warning = warnings.contains(&warning);
            if existing_warning && !failed_check {
                warnings.remove(&warning);
            } else if !existing_warning && failed_check {
                warnings.insert(warning);
            }
        } else {
            warnings.clear();

            if PofToolsGui::radius_test_failed(model, None) {
                warnings.insert(Warning::RadiusTooSmall(None));
            }

            for subobj in &model.sub_objects {
                if PofToolsGui::radius_test_failed(model, Some(subobj.obj_id)) {
                    warnings.insert(Warning::RadiusTooSmall(Some(subobj.obj_id)));
                }
            }
        }
    }

    // tests if the radius for a subobjects or the header is too small for its geometry
    // None means the header/entire model's radius
    fn radius_test_failed(model: &Model, subobj_opt: Option<ObjectId>) -> bool {
        if let Some(subobj) = subobj_opt {
            let subobj = &model.sub_objects[subobj.0 as usize];
            let radius_with_margin = (1.0 + f32::EPSILON) * subobj.radius;
            for vert in &subobj.bsp_data.verts {
                if vert.magnitude() > radius_with_margin {
                    return true;
                }
            }
        } else {
            let radius_with_margin = (1.0 + f32::EPSILON) * model.header.max_radius;
            for subobj in &model.sub_objects {
                // we dont care about subobjects which aren't part of the detail0 hierarchy
                if !model.is_obj_id_ancestor(subobj.obj_id, model.header.detail_levels[0]) {
                    continue;
                }

                let offset = model.get_total_subobj_offset(subobj.obj_id);
                for vert in &subobj.bsp_data.verts {
                    if (*vert + offset).magnitude() > radius_with_margin {
                        return true;
                    }
                }
            }
        }

        false
    }
}
