use egui::{text::LayoutJob, Align2, CollapsingHeader, Color32, DragValue, Label, Response, RichText, TextFormat};
use glium::{
    texture::{RawImage2d, SrgbTexture2d},
    Display,
};
use nalgebra_glm::TMat4;
use pof::{
    Dock, EyePoint, GlowPoint, GlowPointBank, Insignia, Model, PathId, PathPoint, SpecialPoint, SubObject, SubsysMovementAxis, TextureId,
    ThrusterGlow, Vec3d, Version, WeaponHardpoint,
};
use std::{
    collections::{BTreeSet, HashMap},
    str::FromStr,
    sync::mpsc::Receiver,
};

use eframe::egui::{self, Button, TextStyle, Ui};
use pof::ObjectId;

use crate::{GlBufferedInsignia, GlBufferedObject, GlBufferedShield, GlLollipops, POF_TOOLS_VERSION};

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

#[derive(Default)]
struct TransformWindow {
    open: bool,
    vector: String,
    value: String,
    axis_select: usize,
    transform_type: TransformType,
}

enum PropertiesPanel {
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
        properties: String,
        rot_axis: SubsysMovementAxis,
        transform_window: TransformWindow,
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
        fvec_string: String,
        uvec_ang: f32,
        properties: String,
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
        properties: String,
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
            properties: Default::default(),
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
            fvec_string: Default::default(),
            uvec_ang: Default::default(),
            properties: Default::default(),
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
            properties: Default::default(),
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

#[derive(PartialEq, Hash, Debug, Clone, Copy)]
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
    VisualCenter,
    Comments,
}
impl std::fmt::Display for TreeSelection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TreeSelection::Header => write!(f, "Treeview - Header"),
            TreeSelection::SubObjects(selection) => write!(f, "Treeview - Subobjects - {}", selection),
            TreeSelection::Textures(selection) => write!(f, "Treeview - Textures - {}", selection),
            TreeSelection::Weapons(selection) => write!(f, "Treeview - Weapons - {}", selection),
            TreeSelection::DockingBays(selection) => write!(f, "Treeview - DockingBays - {}", selection),
            TreeSelection::Thrusters(selection) => write!(f, "Treeview - Thrusters - {}", selection),
            TreeSelection::Glows(selection) => write!(f, "Treeview - Glows - {}", selection),
            TreeSelection::SpecialPoints(selection) => write!(f, "Treeview - SpecialPoints - {}", selection),
            TreeSelection::Turrets(selection) => write!(f, "Treeview - Turrets - {}", selection),
            TreeSelection::Paths(selection) => write!(f, "Treeview - Paths - {}", selection),
            TreeSelection::Shield => write!(f, "Treeview - Shield"),
            TreeSelection::EyePoints(selection) => write!(f, "Treeview - EyePoints - {}", selection),
            TreeSelection::Insignia(selection) => write!(f, "Treeview - Insignia - {}", selection),
            TreeSelection::VisualCenter => write!(f, "Treeview - VisualCenter"),
            TreeSelection::Comments => write!(f, "Treeview - Comments"),
        }
    }
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
    //             TextureSelection::Texture(tex) => &model.textures[tex],
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
#[derive(PartialEq, Hash, Debug, Clone, Copy)]
pub(crate) enum InsigniaSelection {
    Header,
    Insignia(usize), // insignia idx
}
impl std::fmt::Display for InsigniaSelection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InsigniaSelection::Header => write!(f, "Header"),
            InsigniaSelection::Insignia(idx) => write!(f, "{}", idx + 1),
        }
    }
}
impl InsigniaSelection {
    fn _insignia(point: Option<usize>) -> Self {
        match point {
            Some(point) => Self::Insignia(point),
            None => Self::Header,
        }
    }
}

#[derive(PartialEq, Hash, Debug, Clone, Copy)]
pub(crate) enum EyeSelection {
    Header,
    EyePoint(usize), // eye idx
}
impl std::fmt::Display for EyeSelection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EyeSelection::Header => write!(f, "Header"),
            EyeSelection::EyePoint(idx) => write!(f, "{}", idx + 1),
        }
    }
}
impl EyeSelection {
    fn point(point: Option<usize>) -> Self {
        match point {
            Some(point) => Self::EyePoint(point),
            None => Self::Header,
        }
    }
}

#[derive(PartialEq, Hash, Debug, Clone, Copy)]
pub(crate) enum PathSelection {
    Header,
    Path(usize),             // path idx
    PathPoint(usize, usize), // path idx, point idx
}
impl std::fmt::Display for PathSelection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PathSelection::Header => write!(f, "Header"),
            PathSelection::Path(idx) => write!(f, "Path {}", idx + 1),
            PathSelection::PathPoint(idx, idx2) => write!(f, "Path {} Point {}", idx + 1, idx2 + 1),
        }
    }
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

#[derive(PartialEq, Hash, Debug, Clone, Copy)]
pub(crate) enum TurretSelection {
    Header,
    Turret(usize),             // turret idx
    TurretPoint(usize, usize), // turret idx, point idx
}

impl std::fmt::Display for TurretSelection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TurretSelection::Header => write!(f, "Header"),
            TurretSelection::Turret(idx) => write!(f, "Turret {}", idx + 1),
            TurretSelection::TurretPoint(idx, idx2) => write!(f, "Turret {} Point {}", idx + 1, idx2 + 1),
        }
    }
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

#[derive(PartialEq, Hash, Debug, Clone, Copy)]
pub(crate) enum SpecialPointSelection {
    Header,
    Point(usize),
}
impl std::fmt::Display for SpecialPointSelection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SpecialPointSelection::Header => write!(f, "Header"),
            SpecialPointSelection::Point(idx) => write!(f, "Point {}", idx + 1),
        }
    }
}
impl SpecialPointSelection {
    fn point(point: Option<usize>) -> Self {
        match point {
            Some(point) => Self::Point(point),
            None => Self::Header,
        }
    }
}

#[derive(PartialEq, Hash, Debug, Clone, Copy)]
pub(crate) enum GlowSelection {
    Header,
    Bank(usize),             // bank idx
    BankPoint(usize, usize), // bank idx, point idx
}

impl std::fmt::Display for GlowSelection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GlowSelection::Header => write!(f, "Header"),
            GlowSelection::Bank(idx) => write!(f, "Bank {}", idx + 1),
            GlowSelection::BankPoint(idx, idx2) => write!(f, "Bank {} Point {}", idx + 1, idx2 + 1),
        }
    }
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

#[derive(PartialEq, Hash, Debug, Clone, Copy)]
pub(crate) enum DockingSelection {
    Header,
    Bay(usize), // bank idx
}
impl std::fmt::Display for DockingSelection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DockingSelection::Header => write!(f, "Header"),
            DockingSelection::Bay(idx) => write!(f, "Bay {}", idx + 1),
        }
    }
}
impl DockingSelection {
    fn bay(bay: Option<usize>) -> Self {
        match bay {
            Some(bay) => Self::Bay(bay),
            None => Self::Header,
        }
    }
}

#[derive(PartialEq, Hash, Debug, Eq, PartialOrd, Ord, Clone, Copy)]
pub(crate) enum WeaponSelection {
    Header,
    PriHeader,
    PriBank(usize),             // bank idx
    PriBankPoint(usize, usize), // bank idx, point idx
    SecHeader,
    SecBank(usize),             // bank idx
    SecBankPoint(usize, usize), // bank idx, point idx
}
impl std::fmt::Display for WeaponSelection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            WeaponSelection::Header => write!(f, "Header"),
            WeaponSelection::PriHeader => write!(f, "Primary Header"),
            WeaponSelection::PriBank(idx) => write!(f, "Primary Bank {}", idx + 1),
            WeaponSelection::PriBankPoint(idx, idx2) => write!(f, "Primary Bank {} Point {}", idx + 1, idx2 + 1),
            WeaponSelection::SecHeader => write!(f, "Secondary Header"),
            WeaponSelection::SecBank(idx) => write!(f, "Secondary Bank {}", idx + 1),
            WeaponSelection::SecBankPoint(idx, idx2) => write!(f, "Secondary Bank {} Point {}", idx + 1, idx2 + 1),
        }
    }
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

#[derive(PartialEq, Hash, Debug, Clone, Copy)]
pub(crate) enum TextureSelection {
    Header,
    Texture(TextureId),
}
impl std::fmt::Display for TextureSelection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TextureSelection::Header => write!(f, "Header"),
            TextureSelection::Texture(idx) => write!(f, "Texture {}", idx.0 + 1),
        }
    }
}

#[derive(PartialEq, Hash, Debug, Clone, Copy)]
pub(crate) enum ThrusterSelection {
    Header,
    Bank(usize),             // bank idx
    BankPoint(usize, usize), // bank idx, point idx
}
impl std::fmt::Display for ThrusterSelection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ThrusterSelection::Header => write!(f, "Header"),
            ThrusterSelection::Bank(idx) => write!(f, "Bank {}", idx + 1),
            ThrusterSelection::BankPoint(idx, idx2) => write!(f, "Bank {} Point {}", idx + 1, idx2 + 1),
        }
    }
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

#[derive(PartialEq, Hash, Debug, Clone, Copy)]
pub(crate) enum SubObjectSelection {
    Header,
    SubObject(ObjectId),
}

impl std::fmt::Display for SubObjectSelection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SubObjectSelection::Header => write!(f, "Header"),
            SubObjectSelection::SubObject(idx) => write!(f, "SubObject {}", idx.0 + 1),
        }
    }
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
pub enum Error {
    InvalidTurretGunSubobject(usize), // turret index
    TooManyDebrisObjects,
    // detail# not at top of hierarchy
    // all turret base/gun objects must be disjoint!
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Copy, Clone)]
pub(crate) enum Warning {
    RadiusTooSmall(Option<ObjectId>),
    BBoxTooSmall(Option<ObjectId>),
    DockingBayWithoutPath(usize),
    ThrusterPropertiesInvalidVersion(usize),
    WeaponOffsetInvalidVersion(WeaponSelection),
    // path with no parent
    // thruster with no engine subsys
    // turret uvec != turret normal
    // subobject vert/norm overbudget
    // turret subobject properties not set up for a turret
    // untextured polygons
}

#[derive(PartialEq, Eq)]
pub(crate) enum DisplayMode {
    Wireframe,
    Untextured,
    Textured,
}

#[derive(Default)]
pub(crate) struct UiState {
    pub tree_view_selection: TreeSelection,
    pub viewport_3d_dirty: bool,
    pub last_selected_subobj: Option<ObjectId>,
    properties_panel: PropertiesPanel,
    pub display_radius: bool,
    pub display_bbox: bool,
    pub display_origin: bool,
}

pub(crate) struct PofToolsGui {
    pub model: Box<Model>,
    pub model_loading_thread: Option<Receiver<Result<Option<Box<Model>>, String>>>,
    pub texture_loading_thread: Option<Receiver<Option<(RawImage2d<'static, u8>, TextureId)>>>,
    pub glow_point_sim_start: std::time::Instant,

    pub ui_state: UiState,
    pub display_mode: DisplayMode,
    pub glow_point_simulation: bool,
    pub warnings: BTreeSet<Warning>,
    pub errors: BTreeSet<Error>,

    pub camera_pitch: f32,
    pub camera_heading: f32,
    pub camera_scale: f32,
    pub camera_offset: Vec3d,

    pub buffer_objects: Vec<GlBufferedObject>, // all the subobjects, conditionally rendered based on the current tree selection
    pub buffer_textures: HashMap<TextureId, SrgbTexture2d>, // map of tex ids to actual textures
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
    pub fn new() -> Self {
        Self {
            model: Default::default(),
            model_loading_thread: Default::default(),
            texture_loading_thread: Default::default(),
            glow_point_sim_start: std::time::Instant::now(),
            ui_state: Default::default(),
            display_mode: DisplayMode::Textured,
            glow_point_simulation: Default::default(),
            warnings: Default::default(),
            errors: Default::default(),
            camera_pitch: Default::default(),
            camera_heading: Default::default(),
            camera_scale: Default::default(),
            camera_offset: Default::default(),
            buffer_objects: Default::default(),
            buffer_textures: Default::default(),
            buffer_shield: Default::default(),
            buffer_insignias: Default::default(),
            lollipops: Default::default(),
        }
    }

    fn tree_selectable_item(&mut self, ui: &mut Ui, name: &str, selection: TreeSelection) {
        self.ui_state.tree_selectable_item(&self.model, ui, name, selection);
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
        ui.visuals_mut().widgets.hovered.fg_stroke.color = Default::default();
        ui.visuals_mut().widgets.inactive.fg_stroke.color = Default::default();
        ui.visuals_mut().widgets.active.fg_stroke.color = Default::default();
        ui.visuals_mut().widgets.open.fg_stroke.color = Default::default();
    }

    fn tree_selectable_item(&mut self, model: &Model, ui: &mut Ui, name: &str, selection: TreeSelection) {
        if ui.selectable_value(&mut self.tree_view_selection, selection, name).clicked() {
            self.refresh_properties_panel(model);
            self.viewport_3d_dirty = true;

            info!("Switched to {}", self.tree_view_selection);

            // maybe update ast selected object
            if let TreeSelection::SubObjects(SubObjectSelection::SubObject(id)) = self.tree_view_selection {
                self.last_selected_subobj = Some(id);
            } else if let TreeSelection::SubObjects(SubObjectSelection::Header) | TreeSelection::Header = self.tree_view_selection {
                self.last_selected_subobj = model.header.detail_levels.first().copied();
            }
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

            info!("Switched to {}", self.tree_view_selection);

            // maybe update last selected object
            if let TreeSelection::SubObjects(SubObjectSelection::SubObject(id)) = self.tree_view_selection {
                self.last_selected_subobj = Some(id);
            } else if let TreeSelection::SubObjects(SubObjectSelection::Header) | TreeSelection::Header = self.tree_view_selection {
                self.last_selected_subobj = model.header.detail_levels.first().copied();
            }
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
    fn subobject_combo_box(
        ui: &mut Ui, name_list: &[String], mut_selection: &mut usize, selector_value: Option<usize>, label: &str, active_error_idx: Option<usize>,
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
                    .selected_text(name_list[*mut_selection].clone())
                    .show_ui(ui, |ui| {
                        for (i, name) in name_list.iter().enumerate() {
                            if active_error_idx == Some(i) {
                                // change the color of this entry to red if its the error one
                                ui.visuals_mut().override_text_color = Some(color);
                            }
                            ui.selectable_value(mut_selection, i, name);
                            ui.visuals_mut().override_text_color = None;
                        }
                    });
                ret = Some(*mut_selection);

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
            .anchor(Align2::RIGHT_TOP, [-100.0, 100.0]);

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
                        properties: format!("{}", model.sub_objects[id].properties),
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
                        position_string: format!("{}", model.docking_bays[bay].position),
                        fvec_string: format!("{}", model.docking_bays[bay].fvec.0),
                        uvec_ang: model.docking_bays[bay].get_uvec_angle().to_degrees() % 360.0,
                        properties: format!("{}", model.docking_bays[bay].properties),
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
                        glow_texture_string: format!("{}", model.glow_banks[bank].get_glow_texture().unwrap_or_default()),
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
                        glow_texture_string: format!("{}", model.glow_banks[bank].get_glow_texture().unwrap_or_default()),
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
                        properties: format!("{}", model.special_points[point].properties),
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

impl PofToolsGui {
    // =====================================================
    // The big top-level function for drawing and interacting with all of the UI
    // ====================================================
    pub fn show_ui(&mut self, ctx: &egui::Context, display: &Display) {
        egui::TopBottomPanel::top("menu").default_height(33.0).min_height(33.0).show(ctx, |ui| {
            Ui::add_space(ui, 6.0);
            ui.horizontal(|ui| {
                if ui
                    .add(Button::new(RichText::new("🗁").text_style(TextStyle::Heading)))
                    .on_hover_text("Open")
                    .clicked()
                {
                    self.start_loading_model(None);
                    ui.output().cursor_icon = egui::CursorIcon::Wait;
                }

                if ui
                    .add_enabled(self.errors.is_empty(), Button::new(RichText::new("🖴").text_style(TextStyle::Heading)))
                    .on_hover_text("Save")
                    .on_disabled_hover_text("All errors must be corrected before saving.")
                    .clicked()
                {
                    self.model.clean_up();

                    let new_filename = PofToolsGui::save_model(&self.model);
                    if let Some(filename) = new_filename {
                        display
                            .gl_window()
                            .window()
                            .set_title(&format!("Pof Tools v{} - {}", POF_TOOLS_VERSION, filename));
                    }
                }

                ui.separator();

                ui.menu_button(RichText::new(format!("Version: {}", self.model.version)).text_style(TextStyle::Button), |ui| {
                    let mut response = ui
                        .radio_value(&mut self.model.version, Version::V21_16, "21.16")
                        .on_hover_text("Retail - PCS2 Compatible");
                    response = response.union(
                        ui.radio_value(&mut self.model.version, Version::V21_17, "21.17")
                            .on_hover_text("Retail - PCS2 Compatible - Thruster properties added"),
                    );
                    response = response.union(
                        ui.radio_value(&mut self.model.version, Version::V21_18, "21.18")
                            .on_hover_text("External weapon angle offset added"),
                    );
                    response = response.union(
                        ui.radio_value(&mut self.model.version, Version::V22_00, "22.00")
                            .on_hover_text("SLC2 replaces SLDC (no weapon offset compatibility)"),
                    );
                    response = response.union(
                        ui.radio_value(&mut self.model.version, Version::V22_01, "22.01")
                            .on_hover_text("External weapon angle offset compatible"),
                    );

                    // we only need to recheck verson-specific warnings, but since those are parameterized, there's no easy way to say
                    // 'those specific warnings but for all their parameters' so just do them all i guess
                    if response.changed() {
                        PofToolsGui::recheck_warnings(&mut self.warnings, &self.model, All);
                    }
                });

                ui.separator();

                ui.scope(|ui| {
                    if self.display_mode == DisplayMode::Textured {
                        ui.visuals_mut().widgets.inactive.bg_stroke = ui.visuals().widgets.hovered.bg_stroke;
                    }
                    if ui.add(Button::new(RichText::new("\u{2593}").text_style(TextStyle::Heading))).clicked() {
                        self.display_mode = DisplayMode::Textured;
                    }
                });

                ui.scope(|ui| {
                    if self.display_mode == DisplayMode::Untextured {
                        ui.visuals_mut().widgets.inactive.bg_stroke = ui.visuals().widgets.hovered.bg_stroke;
                    }
                    if ui.add(Button::new(RichText::new("⏹").text_style(TextStyle::Heading))).clicked() {
                        self.display_mode = DisplayMode::Untextured;
                    }
                });

                ui.scope(|ui| {
                    if self.display_mode == DisplayMode::Wireframe {
                        ui.visuals_mut().widgets.inactive.bg_stroke = ui.visuals().widgets.hovered.bg_stroke;
                    }
                    if ui.add(Button::new(RichText::new("⛶").text_style(TextStyle::Heading))).clicked() {
                        self.display_mode = DisplayMode::Wireframe;
                    }
                });

                ui.add_space(ui.available_width() - ui.spacing().interact_size.x / 2.0);

                if self.model_loading_thread.is_some() || self.texture_loading_thread.is_some() {
                    ui.add(egui::widgets::Spinner::new());
                }
            });
        });
        let mut warnings = egui::TopBottomPanel::bottom("info bar")
            .resizable(true)
            .default_height(16.0)
            .height_range(16.0..=500.0)
            .show(ctx, |ui| {
                egui::ScrollArea::vertical()
                    .auto_shrink([false, false])
                    .min_scrolled_height(10.0)
                    .show(ui, |ui| {
                        for error in &self.errors {
                            match *error {
                                Error::InvalidTurretGunSubobject(turret_num) => {
                                    let turret_name = if self.model.sub_objects[self.model.turrets[turret_num].base_obj]
                                        .name
                                        .to_lowercase()
                                        .starts_with("turret")
                                    {
                                        ""
                                    } else {
                                        "turret "
                                    };
                                    let str = format!(
                                        "⊗ {}{} has an invalid gun object",
                                        turret_name, self.model.sub_objects[self.model.turrets[turret_num].base_obj].name
                                    );
                                    ui.add(Label::new(RichText::new(str).text_style(TextStyle::Button).color(Color32::RED)));
                                }
                                Error::TooManyDebrisObjects => {
                                    let mut num_debris = 0;
                                    for sobj in &self.model.sub_objects {
                                        if sobj.is_debris_model {
                                            num_debris += 1;
                                        }
                                    }
                                    ui.add(Label::new(
                                        RichText::new(format!(
                                            "⊗ This model has too many debris objects ({}/{})",
                                            num_debris,
                                            pof::MAX_DEBRIS_OBJECTS
                                        ))
                                        .text_style(TextStyle::Button)
                                        .color(Color32::RED),
                                    ));
                                }
                            }
                        }
                        for warning in &self.warnings {
                            match warning {
                                Warning::RadiusTooSmall(id_opt) => {
                                    let str = format!(
                                        "⚠ {}'s radius does not encompass all of its geometry",
                                        id_opt.map_or("The header", |id| &self.model.sub_objects[id].name)
                                    );
                                    ui.add(Label::new(RichText::new(str).text_style(TextStyle::Button).color(Color32::YELLOW)));
                                }
                                Warning::BBoxTooSmall(id_opt) => {
                                    let str = format!(
                                        "⚠ {}'s bounding box does not encompass all of its geometry",
                                        id_opt.map_or("The header", |id| &self.model.sub_objects[id].name)
                                    );
                                    ui.add(Label::new(RichText::new(str).text_style(TextStyle::Button).color(Color32::YELLOW)));
                                }
                                Warning::DockingBayWithoutPath(bay_num) => {
                                    let str = format!(
                                        "⚠ Docking bay {} cannot be used by ships without a path",
                                        self.model.docking_bays[*bay_num].get_name().unwrap_or(&(bay_num + 1).to_string())
                                    );
                                    ui.add(Label::new(RichText::new(str).text_style(TextStyle::Button).color(Color32::YELLOW)));
                                }
                                Warning::ThrusterPropertiesInvalidVersion(idx) => {
                                    let str =
                                        format!("⚠ Thruster bank {} has properties, which the currently selected version does not support", idx + 1);
                                    ui.add(Label::new(RichText::new(str).text_style(TextStyle::Button).color(Color32::YELLOW)));
                                }
                                Warning::WeaponOffsetInvalidVersion(weapon_selection) => {
                                    let str = format!(
                                        "⚠ {} has an external angle offset, which the currently selected version does not support",
                                        weapon_selection
                                    );
                                    ui.add(Label::new(RichText::new(str).text_style(TextStyle::Button).color(Color32::YELLOW)));
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
                                        for &i in &obj.children {
                                            make_subobject_child_list(ui_state, model, &model.sub_objects[i], ui)
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
                                ui_state.tree_selectable_item(
                                    &self.model,
                                    ui,
                                    docking_bay.get_name().unwrap_or(&format!("Bay {}", i + 1)),
                                    TreeSelection::DockingBays(DockingSelection::Bay(i)),
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
                                    &format!(
                                        "Bank {}{}",
                                        i + 1,
                                        self.model.glow_banks[i]
                                            .get_glow_texture()
                                            .map_or(String::new(), |tex| format!(" ({})", tex))
                                    ),
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
                                    &self.model.sub_objects[turret.base_obj].name,
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
                                    &format!("{} {}", self.model.sub_objects[eye.attached_subobj].name, i + 1),
                                    TreeSelection::EyePoints(EyeSelection::EyePoint(i)),
                                );
                            }
                        },
                    );

                    self.ui_state
                        .tree_selectable_item(&self.model, ui, "Visual Center", TreeSelection::VisualCenter);

                    self.ui_state.tree_selectable_item(&self.model, ui, "Comments", TreeSelection::Comments);
                });
            });

        // ==============================================================================================================
        // The 'properties panel' is the section on the right of the UI which contains the specific fields ready for
        // manipulation of whatever it is they have selected in the tree view
        // ==============================================================================================================

        // reset some stuff
        self.display_bbox = false;
        self.display_radius = false;
        self.display_origin = false;

        egui::SidePanel::right("properties_panel")
            .resizable(true)
            .default_width(200.0)
            .width_range(200.0..=500.0)
            .show(ctx, |ui| {
                egui::ScrollArea::vertical().auto_shrink([false, false]).show(ui, |ui| {
                    let mut reload_textures = false;
                    let mut properties_panel_dirty = false;

                    ui.add_space(3.0);

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

                            if ui.add(egui::Button::new("Transform")).clicked() {
                                transform_window.open = true;
                            }
                            if let Some(matrix) = UiState::show_transform_window(ctx, transform_window) {
                                for i in 0..self.model.sub_objects.len() {
                                    // only apply to top-level subobjects (no parent), apply_transform() will
                                    // recursively apply the proper transform to its children
                                    if self.model.sub_objects[ObjectId(i as u32)].parent == None {
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
                            UiState::model_value_edit(
                                &mut self.ui_state.viewport_3d_dirty,
                                ui,
                                false,
                                Some(&mut self.model.header.mass),
                                mass_string,
                            );

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
                            transform_window,
                        } => {
                            ui.heading("SubObject");
                            ui.separator();

                            let selected_id = if let TreeSelection::SubObjects(SubObjectSelection::SubObject(id)) = self.ui_state.tree_view_selection
                            {
                                Some(id)
                            } else {
                                None
                            };

                            if ui.add_enabled(matches!(selected_id, Some(_)), egui::Button::new("Transform")).clicked() {
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

                            ui.label("Name:");
                            if let Some(id) = selected_id {
                                ui.add(egui::TextEdit::singleline(&mut self.model.sub_objects[id].name));
                            } else {
                                ui.add_enabled(false, egui::TextEdit::singleline(name));
                            }

                            ui.add_space(5.0);

                            let num_debris = self.model.num_debris_objects();
                            ui.add_enabled_ui(
                                selected_id.is_some()
                                    && (num_debris < pof::MAX_DEBRIS_OBJECTS || self.model.sub_objects[selected_id.unwrap()].is_debris_model),
                                |ui| {
                                    if selected_id.map_or(false, |id| !self.model.sub_objects[id].is_debris_model)
                                        && num_debris >= pof::MAX_DEBRIS_OBJECTS
                                    {
                                        UiState::set_widget_color(ui, Color32::RED);
                                    }
                                    if ui
                                        .checkbox(is_debris_check, "Debris Subobject")
                                        .on_disabled_hover_text(format!("The Maximum number of Debris is {}", pof::MAX_DEBRIS_OBJECTS))
                                        .changed()
                                    {
                                        self.model.sub_objects[selected_id.unwrap()].is_debris_model = *is_debris_check;
                                        PofToolsGui::recheck_errors(&mut self.errors, &self.model, One(Error::TooManyDebrisObjects));
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

                            ui.label("Properties:");
                            if let Some(id) = selected_id {
                                ui.add(egui::TextEdit::multiline(&mut self.model.sub_objects[id].properties).desired_rows(2));
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
                                if self.warnings.contains(&Warning::ThrusterPropertiesInvalidVersion(bank)) {
                                    UiState::set_widget_color(ui, Color32::YELLOW);
                                }
                                if ui
                                    .add(egui::TextEdit::multiline(&mut self.model.thruster_banks[bank].properties).desired_rows(1))
                                    .changed()
                                {
                                    PofToolsGui::recheck_warnings(
                                        &mut self.warnings,
                                        &self.model,
                                        One(Warning::ThrusterPropertiesInvalidVersion(bank)),
                                    );
                                }
                                UiState::reset_widget_color(ui);
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
                            println!("{}", radius_string);
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

                                self.ui_state.tree_view_selection =
                                    TreeSelection::Thrusters(ThrusterSelection::bank_point(bank_num.unwrap(), new_idx));
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

                                self.ui_state.tree_view_selection =
                                    TreeSelection::Weapons(WeaponSelection::bank_point(is_primary, bank_num.unwrap(), new_idx));
                                properties_panel_dirty = true;
                                self.ui_state.viewport_3d_dirty = true;
                            }

                            if offset_changed {
                                PofToolsGui::recheck_warnings(
                                    &mut self.warnings,
                                    &self.model,
                                    One(Warning::WeaponOffsetInvalidVersion(weapon_selection)),
                                );
                            }
                        }
                        PropertiesPanel::DockingBay { position_string, fvec_string, uvec_ang, properties, path_num } => {
                            ui.heading("Docking Bay");
                            ui.separator();

                            let bay_num = match self.ui_state.tree_view_selection {
                                TreeSelection::DockingBays(DockingSelection::Bay(bay)) => Some(bay),
                                _ => None,
                            };

                            let bay_idx_response = UiState::list_manipulator_widget(ui, bay_num, Some(self.model.docking_bays.len()), "Bay");

                            ui.add_space(10.0);

                            ui.label("Properties:");
                            if let Some(bay) = bay_num {
                                ui.add(egui::TextEdit::multiline(&mut self.model.docking_bays[bay].properties).desired_rows(1));
                            } else {
                                ui.add_enabled(false, egui::TextEdit::multiline(properties).desired_rows(1));
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
                                if ui.add(egui::TextEdit::multiline(glow_texture_string).desired_rows(1)).changed() {
                                    self.model.glow_banks[bank].set_glow_texture(glow_texture_string);
                                }
                            } else {
                                ui.add_enabled(false, egui::TextEdit::multiline(glow_texture_string).desired_rows(1));
                            }

                            let subobj_names_list = self.model.get_subobj_names();

                            if let Some(new_subobj) =
                                UiState::subobject_combo_box(ui, &subobj_names_list, attached_subobj_idx, bank_num, "SubObject", None)
                            {
                                self.model.glow_banks[bank_num.unwrap()].obj_parent = ObjectId(new_subobj as u32);
                            }

                            let (disp_time, on_time, off_time, lod, glow_type, glow_points) =
                                if let TreeSelection::Glows(GlowSelection::BankPoint(bank, _)) = self.ui_state.tree_view_selection {
                                    let GlowPointBank {
                                        disp_time, on_time, off_time, lod, glow_type, glow_points, ..
                                    } = &mut self.model.glow_banks[bank as usize];

                                    (Some(disp_time), Some(on_time), Some(off_time), Some(lod), Some(glow_type), Some(glow_points))
                                } else if let TreeSelection::Glows(GlowSelection::Bank(bank)) = self.ui_state.tree_view_selection {
                                    let GlowPointBank {
                                        disp_time, on_time, off_time, lod, glow_type, glow_points, ..
                                    } = &mut self.model.glow_banks[bank as usize];

                                    (Some(disp_time), Some(on_time), Some(off_time), Some(lod), Some(glow_type), Some(glow_points))
                                } else {
                                    (None, None, None, None, None, None)
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
                                properties_panel_dirty = true;
                                self.ui_state.viewport_3d_dirty = true;
                            } else if let Some(response) = point_idx_response {
                                let new_idx = response.apply(&mut self.model.glow_banks[bank_num.unwrap()].glow_points);

                                self.ui_state.tree_view_selection = TreeSelection::Glows(GlowSelection::bank_point(bank_num.unwrap(), new_idx));
                                properties_panel_dirty = true;
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

                            if let Some(new_subobj) = UiState::subobject_combo_box(ui, &subobj_names_list, base_idx, turret_num, "Base object", None)
                            {
                                self.model.turrets[turret_num.unwrap()].base_obj = ObjectId(new_subobj as u32);
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
                                PofToolsGui::recheck_errors(
                                    &mut self.errors,
                                    &self.model,
                                    One(Error::InvalidTurretGunSubobject(turret_num.unwrap())),
                                );
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
                                properties_panel_dirty = true;
                                self.ui_state.viewport_3d_dirty = true;
                            } else if let Some(response) = point_idx_response {
                                let new_idx = response.apply(&mut self.model.turrets[turret_num.unwrap()].fire_points);

                                self.ui_state.tree_view_selection =
                                    TreeSelection::Turrets(TurretSelection::turret_point(turret_num.unwrap(), new_idx));
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
                });
            });
    }

    // rechecks just one or all of the warnings on the model
    pub fn recheck_errors(errors: &mut BTreeSet<Error>, model: &Model, error_to_check: Set<Error>) {
        if let One(error) = error_to_check {
            let failed_check = match error {
                Error::InvalidTurretGunSubobject(turret) => PofToolsGui::turret_gun_subobj_not_valid(model, turret),
                Error::TooManyDebrisObjects => model.num_debris_objects() > pof::MAX_DEBRIS_OBJECTS,
            };

            let existing_warning = errors.contains(&error);
            if existing_warning && !failed_check {
                errors.remove(&error);
            } else if !existing_warning && failed_check {
                errors.insert(error);
            }
        } else {
            errors.clear();

            for i in 0..model.turrets.len() {
                if PofToolsGui::turret_gun_subobj_not_valid(model, i) {
                    errors.insert(Error::InvalidTurretGunSubobject(i));
                }
            }

            if model.num_debris_objects() > pof::MAX_DEBRIS_OBJECTS {
                errors.insert(Error::TooManyDebrisObjects);
            }
        }
    }

    fn turret_gun_subobj_not_valid(model: &Model, turret_num: usize) -> bool {
        let turret = &model.turrets[turret_num];
        if turret.base_obj == turret.gun_obj {
            return false;
        }

        for &child_id in &model.sub_objects[turret.base_obj].children {
            if child_id == turret.gun_obj {
                return false;
            }
        }

        true
    }

    // rechecks just one or all of the warnings on the model
    pub(crate) fn recheck_warnings(warnings: &mut BTreeSet<Warning>, model: &Model, warning_to_check: Set<Warning>) {
        if let One(warning) = warning_to_check {
            let failed_check = match warning {
                Warning::RadiusTooSmall(subobj_opt) => PofToolsGui::radius_test_failed(model, subobj_opt),
                Warning::BBoxTooSmall(subobj_opt) => PofToolsGui::bbox_test_failed(model, subobj_opt),
                Warning::DockingBayWithoutPath(bay_num) => model.docking_bays[bay_num].path.is_none(),
                Warning::ThrusterPropertiesInvalidVersion(bank_idx) => {
                    model.version <= Version::V21_16 && !model.thruster_banks[bank_idx].properties.is_empty()
                }
                Warning::WeaponOffsetInvalidVersion(weapon_select) => {
                    (model.version <= Version::V21_17 || model.version == Version::V22_00) && {
                        if let WeaponSelection::PriBankPoint(bank, point) = weapon_select {
                            model.primary_weps[bank][point].offset != 0.0
                        } else if let WeaponSelection::SecBankPoint(bank, point) = weapon_select {
                            model.secondary_weps[bank][point].offset != 0.0
                        } else {
                            false
                        }
                    }
                }
            };

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

            if PofToolsGui::bbox_test_failed(model, None) {
                warnings.insert(Warning::BBoxTooSmall(None));
            }
            for subobj in &model.sub_objects {
                if PofToolsGui::bbox_test_failed(model, Some(subobj.obj_id)) {
                    warnings.insert(Warning::BBoxTooSmall(Some(subobj.obj_id)));
                }
            }

            for (i, dock) in model.docking_bays.iter().enumerate() {
                if dock.path.is_none() {
                    warnings.insert(Warning::DockingBayWithoutPath(i));
                }
            }

            if model.version <= Version::V21_16 {
                for (i, bank) in model.thruster_banks.iter().enumerate() {
                    if !bank.properties.is_empty() {
                        warnings.insert(Warning::ThrusterPropertiesInvalidVersion(i));
                    }
                }
            }

            if model.version <= Version::V21_17 || model.version == Version::V22_00 {
                for (i, bank) in model.primary_weps.iter().enumerate() {
                    for (j, point) in bank.iter().enumerate() {
                        if point.offset != 0.0 {
                            warnings.insert(Warning::WeaponOffsetInvalidVersion(WeaponSelection::PriBankPoint(i, j)));
                        }
                    }
                }
                for (i, bank) in model.secondary_weps.iter().enumerate() {
                    for (j, point) in bank.iter().enumerate() {
                        if point.offset != 0.0 {
                            warnings.insert(Warning::WeaponOffsetInvalidVersion(WeaponSelection::SecBankPoint(i, j)));
                        }
                    }
                }
            }
        }
    }

    // tests if the radius for a subobject or the header is too small for its geometry
    // None means the header/entire model's radius
    fn radius_test_failed(model: &Model, subobj_opt: Option<ObjectId>) -> bool {
        if let Some(subobj) = subobj_opt {
            let subobj = &model.sub_objects[subobj];
            let radius_with_margin = (1.0 + f32::EPSILON) * subobj.radius;
            for vert in &subobj.bsp_data.verts {
                if vert.magnitude() > radius_with_margin {
                    return true;
                }
            }
        } else {
            let radius_with_margin = (1.0 + f32::EPSILON) * model.header.max_radius;
            if let Some(&detail_0) = model.header.detail_levels.first() {
                for subobj in &model.sub_objects {
                    // we dont care about subobjects which aren't part of the detail0 hierarchy
                    if !model.is_obj_id_ancestor(subobj.obj_id, detail_0) {
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
        }

        false
    }

    // tests if the bbox for a subobject or the header is too small for its geometry
    // None means the header/entire model's radius
    fn bbox_test_failed(model: &Model, subobj_opt: Option<ObjectId>) -> bool {
        if let Some(subobj) = subobj_opt {
            let subobj = &model.sub_objects[subobj];
            for vert in &subobj.bsp_data.verts {
                if !subobj.bbox.contains(*vert) {
                    return true;
                }
            }
        } else if let Some(&detail_0) = model.header.detail_levels.first() {
            for subobj in &model.sub_objects {
                // we dont care about subobjects which aren't part of the detail0 hierarchy
                if !model.is_obj_id_ancestor(subobj.obj_id, detail_0) {
                    continue;
                }

                for vert in &subobj.bsp_data.verts {
                    if !model.header.bbox.contains(*vert) {
                        return true;
                    }
                }
            }
        }

        false
    }
}
