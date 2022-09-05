use egui::{collapsing_header::CollapsingState, Color32, Id, Label, RichText};
use glium::{
    texture::{RawImage2d, SrgbTexture2d},
    Display,
};
use pof::{Error, Model, SubObject, TextureId, Vec3d, Version, Warning};
use std::{collections::HashMap, sync::mpsc::Receiver};

use eframe::egui::{self, Button, TextStyle, Ui};
use pof::ObjectId;

use crate::{ui_properties_panel::PropertiesPanel, GlBufferedInsignia, GlBufferedShield, GlLollipops, GlArrowhead, GlObjectBuffers, POF_TOOLS_VERSION};

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy, PartialOrd, Ord)]
pub(crate) enum TreeValue {
    Header,
    SubObjects(SubObjectTreeValue),
    Textures(TextureTreeValue),
    Weapons(WeaponTreeValue),
    DockingBays(DockingTreeValue),
    Thrusters(ThrusterTreeValue),
    Glows(GlowTreeValue),
    SpecialPoints(SpecialPointTreeValue),
    Turrets(TurretTreeValue),
    Paths(PathTreeValue),
    Shield,
    EyePoints(EyeTreeValue),
    Insignia(InsigniaTreeValue),
    VisualCenter,
    Comments,
}
impl std::fmt::Display for TreeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TreeValue::Header => write!(f, "Treeview - Header"),
            TreeValue::SubObjects(selection) => write!(f, "Treeview - Subobjects - {}", selection),
            TreeValue::Textures(selection) => write!(f, "Treeview - Textures - {}", selection),
            TreeValue::Weapons(selection) => write!(f, "Treeview - Weapons - {}", selection),
            TreeValue::DockingBays(selection) => write!(f, "Treeview - DockingBays - {}", selection),
            TreeValue::Thrusters(selection) => write!(f, "Treeview - Thrusters - {}", selection),
            TreeValue::Glows(selection) => write!(f, "Treeview - Glows - {}", selection),
            TreeValue::SpecialPoints(selection) => write!(f, "Treeview - SpecialPoints - {}", selection),
            TreeValue::Turrets(selection) => write!(f, "Treeview - Turrets - {}", selection),
            TreeValue::Paths(selection) => write!(f, "Treeview - Paths - {}", selection),
            TreeValue::Shield => write!(f, "Treeview - Shield"),
            TreeValue::EyePoints(selection) => write!(f, "Treeview - EyePoints - {}", selection),
            TreeValue::Insignia(selection) => write!(f, "Treeview - Insignia - {}", selection),
            TreeValue::VisualCenter => write!(f, "Treeview - VisualCenter"),
            TreeValue::Comments => write!(f, "Treeview - Comments"),
        }
    }
}
impl Default for TreeValue {
    fn default() -> Self {
        Self::Header
    }
}
impl TreeValue {
    // returns what, if any, tree_value best corresponds to a given error
    fn from_error(error: Error) -> Option<TreeValue> {
        match error {
            Error::InvalidTurretGunSubobject(idx) => Some(TreeValue::Turrets(TurretTreeValue::Turret(idx))),
            Error::TooManyDebrisObjects => None,
            Error::DetailObjWithParent(id) => Some(TreeValue::SubObjects(SubObjectTreeValue::SubObject(id))),
            Error::DetailAndDebrisObj(id) => Some(TreeValue::SubObjects(SubObjectTreeValue::SubObject(id))),
            Error::TooManyVerts(id) | Error::TooManyNorms(id) | Error::UnnamedSubObject(id) | Error::DuplicateSubobjectName(id) => {
                Some(TreeValue::SubObjects(SubObjectTreeValue::SubObject(id)))
            }
        }
    }

    // returns what, if any, tree_value best corresponds to a given warning
    fn from_warning(warning: Warning) -> Option<TreeValue> {
        match warning {
            Warning::RadiusTooSmall(None) => Some(TreeValue::Header),
            Warning::BBoxTooSmall(None) => Some(TreeValue::Header),
            Warning::InvertedBBox(None) => Some(TreeValue::Header),
            Warning::RadiusTooSmall(Some(id)) => Some(TreeValue::SubObjects(SubObjectTreeValue::SubObject(id))),
            Warning::BBoxTooSmall(Some(id)) => Some(TreeValue::SubObjects(SubObjectTreeValue::SubObject(id))),
            Warning::InvertedBBox(Some(id)) => Some(TreeValue::SubObjects(SubObjectTreeValue::SubObject(id))),
            Warning::SubObjectTranslationInvalidVersion(id) => Some(TreeValue::SubObjects(SubObjectTreeValue::SubObject(id))),
            Warning::UntexturedPolygons => None,
            Warning::DockingBayWithoutPath(idx) => Some(TreeValue::DockingBays(DockingTreeValue::Bay(idx))),
            Warning::ThrusterPropertiesInvalidVersion(idx) => Some(TreeValue::Thrusters(ThrusterTreeValue::Bank(idx))),
            Warning::WeaponOffsetInvalidVersion { primary, bank, point } => {
                if primary {
                    Some(TreeValue::Weapons(WeaponTreeValue::PriBankPoint(bank, point)))
                } else {
                    Some(TreeValue::Weapons(WeaponTreeValue::SecBankPoint(bank, point)))
                }
            }
            Warning::TooFewTurretFirePoints(idx) => Some(TreeValue::Turrets(TurretTreeValue::Turret(idx))),
            Warning::TooManyTurretFirePoints(idx) => Some(TreeValue::Turrets(TurretTreeValue::Turret(idx))),
            Warning::DuplicatePathName(idx) => Some(TreeValue::Paths(PathTreeValue::Path(idx))),
            Warning::DuplicateDetailLevel(_) => Some(TreeValue::Header),
            Warning::TooManyEyePoints => Some(TreeValue::EyePoints(EyeTreeValue::Header)),
            Warning::TooManyTextures => Some(TreeValue::Textures(TextureTreeValue::Header)),
            Warning::PathNameTooLong(idx) => Some(TreeValue::Paths(PathTreeValue::Path(idx))),
            Warning::SpecialPointNameTooLong(idx) => Some(TreeValue::SpecialPoints(SpecialPointTreeValue::Point(idx))),
            Warning::SubObjectNameTooLong(id) => Some(TreeValue::SubObjects(SubObjectTreeValue::SubObject(id))),
            Warning::DockingBayNameTooLong(idx) => Some(TreeValue::DockingBays(DockingTreeValue::Bay(idx))),
            Warning::SubObjectPropertiesTooLong(id) => Some(TreeValue::SubObjects(SubObjectTreeValue::SubObject(id))),
            Warning::ThrusterPropertiesTooLong(idx) => Some(TreeValue::Thrusters(ThrusterTreeValue::Bank(idx))),
            Warning::DockingBayPropertiesTooLong(idx) => Some(TreeValue::DockingBays(DockingTreeValue::Bay(idx))),
            Warning::GlowBankPropertiesTooLong(idx) => Some(TreeValue::Glows(GlowTreeValue::Bank(idx))),
            Warning::SpecialPointPropertiesTooLong(idx) => Some(TreeValue::SpecialPoints(SpecialPointTreeValue::Point(idx))),
        }
    }

    fn is_ancestor_of(self, maybe_descendant: TreeValue) -> bool {
        if self == maybe_descendant {
            return false;
        }
        match (self, maybe_descendant) {
            (TreeValue::SubObjects(SubObjectTreeValue::Header), TreeValue::SubObjects(_)) => true,
            (TreeValue::Textures(TextureTreeValue::Header), TreeValue::Textures(_)) => true,
            (TreeValue::Weapons(WeaponTreeValue::Header), TreeValue::Weapons(_)) => true,
            (TreeValue::Weapons(WeaponTreeValue::PriHeader), TreeValue::Weapons(WeaponTreeValue::PriBank(_))) => true,
            (TreeValue::Weapons(WeaponTreeValue::PriHeader), TreeValue::Weapons(WeaponTreeValue::PriBankPoint(..))) => true,
            (TreeValue::Weapons(WeaponTreeValue::PriBank(idx)), TreeValue::Weapons(WeaponTreeValue::PriBankPoint(idx2, _))) => idx == idx2,
            (TreeValue::Weapons(WeaponTreeValue::SecHeader), TreeValue::Weapons(WeaponTreeValue::SecBank(_))) => true,
            (TreeValue::Weapons(WeaponTreeValue::SecHeader), TreeValue::Weapons(WeaponTreeValue::SecBankPoint(..))) => true,
            (TreeValue::Weapons(WeaponTreeValue::SecBank(idx)), TreeValue::Weapons(WeaponTreeValue::SecBankPoint(idx2, _))) => idx == idx2,
            (TreeValue::DockingBays(DockingTreeValue::Header), TreeValue::DockingBays(_)) => true,
            (TreeValue::Thrusters(ThrusterTreeValue::Header), TreeValue::Thrusters(_)) => true,
            (TreeValue::Thrusters(ThrusterTreeValue::Bank(idx)), TreeValue::Thrusters(ThrusterTreeValue::BankPoint(idx2, _))) => idx == idx2,
            (TreeValue::Glows(GlowTreeValue::Header), TreeValue::Glows(_)) => true,
            (TreeValue::Glows(GlowTreeValue::Bank(idx)), TreeValue::Glows(GlowTreeValue::BankPoint(idx2, _))) => idx == idx2,
            (TreeValue::SpecialPoints(SpecialPointTreeValue::Header), TreeValue::SpecialPoints(_)) => true,
            (TreeValue::Turrets(TurretTreeValue::Header), TreeValue::Turrets(_)) => true,
            (TreeValue::Turrets(TurretTreeValue::Turret(idx)), TreeValue::Turrets(TurretTreeValue::TurretPoint(idx2, _))) => idx == idx2,
            (TreeValue::Paths(PathTreeValue::Header), TreeValue::Paths(_)) => true,
            (TreeValue::Paths(PathTreeValue::Path(idx)), TreeValue::Paths(PathTreeValue::PathPoint(idx2, _))) => idx == idx2,
            (TreeValue::EyePoints(EyeTreeValue::Header), TreeValue::EyePoints(_)) => true,
            (TreeValue::Insignia(InsigniaTreeValue::Header), TreeValue::Insignia(_)) => true,
            _ => false,
        }
    }
}
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy)]
pub(crate) enum InsigniaTreeValue {
    Header,
    Insignia(usize), // insignia idx
}
impl std::fmt::Display for InsigniaTreeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InsigniaTreeValue::Header => write!(f, "Header"),
            InsigniaTreeValue::Insignia(idx) => write!(f, "{}", idx + 1),
        }
    }
}
impl InsigniaTreeValue {
    fn _insignia(point: Option<usize>) -> Self {
        match point {
            Some(point) => Self::Insignia(point),
            None => Self::Header,
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy)]
pub(crate) enum EyeTreeValue {
    Header,
    EyePoint(usize), // eye idx
}
impl std::fmt::Display for EyeTreeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EyeTreeValue::Header => write!(f, "Header"),
            EyeTreeValue::EyePoint(idx) => write!(f, "{}", idx + 1),
        }
    }
}
impl EyeTreeValue {
    pub fn point(point: Option<usize>) -> Self {
        match point {
            Some(point) => Self::EyePoint(point),
            None => Self::Header,
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy)]
pub(crate) enum PathTreeValue {
    Header,
    Path(usize),             // path idx
    PathPoint(usize, usize), // path idx, point idx
}
impl std::fmt::Display for PathTreeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PathTreeValue::Header => write!(f, "Header"),
            PathTreeValue::Path(idx) => write!(f, "Path {}", idx + 1),
            PathTreeValue::PathPoint(idx, idx2) => write!(f, "Path {} Point {}", idx + 1, idx2 + 1),
        }
    }
}
impl PathTreeValue {
    pub fn path_point(path: usize, point: Option<usize>) -> Self {
        match point {
            Some(point) => Self::PathPoint(path, point),
            None => Self::Path(path),
        }
    }
    pub fn path(path: Option<usize>) -> Self {
        match path {
            Some(path) => Self::Path(path),
            None => Self::Header,
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy)]
pub(crate) enum TurretTreeValue {
    Header,
    Turret(usize),             // turret idx
    TurretPoint(usize, usize), // turret idx, point idx
}

impl std::fmt::Display for TurretTreeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TurretTreeValue::Header => write!(f, "Header"),
            TurretTreeValue::Turret(idx) => write!(f, "Turret {}", idx + 1),
            TurretTreeValue::TurretPoint(idx, idx2) => write!(f, "Turret {} Point {}", idx + 1, idx2 + 1),
        }
    }
}
impl TurretTreeValue {
    pub fn turret_point(turret: usize, point: Option<usize>) -> Self {
        match point {
            Some(point) => Self::TurretPoint(turret, point),
            None => Self::Turret(turret),
        }
    }
    pub fn turret(turret: Option<usize>) -> Self {
        match turret {
            Some(turret) => Self::Turret(turret),
            None => Self::Header,
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy)]
pub(crate) enum SpecialPointTreeValue {
    Header,
    Point(usize),
}
impl std::fmt::Display for SpecialPointTreeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SpecialPointTreeValue::Header => write!(f, "Header"),
            SpecialPointTreeValue::Point(idx) => write!(f, "Point {}", idx + 1),
        }
    }
}
impl SpecialPointTreeValue {
    pub fn point(point: Option<usize>) -> Self {
        match point {
            Some(point) => Self::Point(point),
            None => Self::Header,
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy)]
pub(crate) enum GlowTreeValue {
    Header,
    Bank(usize),             // bank idx
    BankPoint(usize, usize), // bank idx, point idx
}

impl std::fmt::Display for GlowTreeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GlowTreeValue::Header => write!(f, "Header"),
            GlowTreeValue::Bank(idx) => write!(f, "Bank {}", idx + 1),
            GlowTreeValue::BankPoint(idx, idx2) => write!(f, "Bank {} Point {}", idx + 1, idx2 + 1),
        }
    }
}
impl GlowTreeValue {
    pub fn bank_point(bank: usize, point: Option<usize>) -> Self {
        match point {
            Some(point) => Self::BankPoint(bank, point),
            None => Self::Bank(bank),
        }
    }
    pub fn bank(bank: Option<usize>) -> Self {
        match bank {
            Some(bank) => Self::Bank(bank),
            None => Self::Header,
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy)]
pub(crate) enum DockingTreeValue {
    Header,
    Bay(usize), // bank idx
}
impl std::fmt::Display for DockingTreeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DockingTreeValue::Header => write!(f, "Header"),
            DockingTreeValue::Bay(idx) => write!(f, "Bay {}", idx + 1),
        }
    }
}
impl DockingTreeValue {
    pub fn bay(bay: Option<usize>) -> Self {
        match bay {
            Some(bay) => Self::Bay(bay),
            None => Self::Header,
        }
    }
}

#[derive(PartialEq, Hash, Debug, Eq, PartialOrd, Ord, Clone, Copy)]
pub(crate) enum WeaponTreeValue {
    Header,
    PriHeader,
    PriBank(usize),             // bank idx
    PriBankPoint(usize, usize), // bank idx, point idx
    SecHeader,
    SecBank(usize),             // bank idx
    SecBankPoint(usize, usize), // bank idx, point idx
}
impl std::fmt::Display for WeaponTreeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            WeaponTreeValue::Header => write!(f, "Header"),
            WeaponTreeValue::PriHeader => write!(f, "Primary Header"),
            WeaponTreeValue::PriBank(idx) => write!(f, "Primary Bank {}", idx + 1),
            WeaponTreeValue::PriBankPoint(idx, idx2) => write!(f, "Primary Bank {} Point {}", idx + 1, idx2 + 1),
            WeaponTreeValue::SecHeader => write!(f, "Secondary Header"),
            WeaponTreeValue::SecBank(idx) => write!(f, "Secondary Bank {}", idx + 1),
            WeaponTreeValue::SecBankPoint(idx, idx2) => write!(f, "Secondary Bank {} Point {}", idx + 1, idx2 + 1),
        }
    }
}
impl WeaponTreeValue {
    pub fn is_primary(self) -> bool {
        matches!(self, Self::PriHeader | Self::PriBank(_) | Self::PriBankPoint(..))
    }
    pub fn bank_point(is_primary: bool, bank: usize, point: Option<usize>) -> Self {
        match (is_primary, point) {
            (true, Some(point)) => Self::PriBankPoint(bank, point),
            (true, None) => Self::PriBank(bank),
            (false, Some(point)) => Self::SecBankPoint(bank, point),
            (false, None) => Self::SecBank(bank),
        }
    }
    pub fn bank(is_primary: bool, bank: Option<usize>) -> Self {
        match (is_primary, bank) {
            (true, Some(bank)) => Self::PriBank(bank),
            (true, None) => Self::PriHeader,
            (false, Some(bank)) => Self::SecBank(bank),
            (false, None) => Self::SecHeader,
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy)]
pub(crate) enum TextureTreeValue {
    Header,
    Texture(TextureId),
}
impl std::fmt::Display for TextureTreeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TextureTreeValue::Header => write!(f, "Header"),
            TextureTreeValue::Texture(idx) => write!(f, "Texture {}", idx.0 + 1),
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy)]
pub(crate) enum ThrusterTreeValue {
    Header,
    Bank(usize),             // bank idx
    BankPoint(usize, usize), // bank idx, point idx
}
impl std::fmt::Display for ThrusterTreeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ThrusterTreeValue::Header => write!(f, "Header"),
            ThrusterTreeValue::Bank(idx) => write!(f, "Bank {}", idx + 1),
            ThrusterTreeValue::BankPoint(idx, idx2) => write!(f, "Bank {} Point {}", idx + 1, idx2 + 1),
        }
    }
}
impl ThrusterTreeValue {
    pub fn bank_point(bank: usize, point: Option<usize>) -> Self {
        match point {
            Some(point) => Self::BankPoint(bank, point),
            None => Self::Bank(bank),
        }
    }
    pub fn bank(bank: Option<usize>) -> Self {
        match bank {
            Some(bank) => Self::Bank(bank),
            None => Self::Header,
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy)]
pub(crate) enum SubObjectTreeValue {
    Header,
    SubObject(ObjectId),
}

impl std::fmt::Display for SubObjectTreeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SubObjectTreeValue::Header => write!(f, "Header"),
            SubObjectTreeValue::SubObject(idx) => write!(f, "SubObject {}", idx.0 + 1),
        }
    }
}

#[derive(PartialEq, Eq)]
pub(crate) enum DisplayMode {
    Wireframe,
    Untextured,
    Textured,
}

#[derive(Default)]
pub(crate) struct UiState {
    pub tree_view_selection: TreeValue,
    /// toggles the expanded state of the given tree value next frame
    pub tree_view_toggle: Option<TreeValue>,
    /// expands the given tree value next frame
    pub tree_view_force_open: Option<TreeValue>,
    pub viewport_3d_dirty: bool,
    pub last_selected_subobj: Option<ObjectId>,
    pub properties_panel: PropertiesPanel,
    pub display_radius: bool,
    pub display_bbox: bool,
    pub display_origin: bool,
    pub display_uvec_fvec: bool,
    pub move_only_offset: bool,
}

pub(crate) struct PofToolsGui {
    pub model: Box<Model>,
    pub model_loading_thread: Option<Receiver<Result<Option<Box<Model>>, String>>>,
    #[allow(clippy::type_complexity)]
    pub texture_loading_thread: Option<Receiver<Option<(RawImage2d<'static, u8>, TextureId)>>>,
    pub glow_point_sim_start: std::time::Instant,

    pub ui_state: UiState,
    pub display_mode: DisplayMode,
    pub glow_point_simulation: bool,

    pub camera_pitch: f32,
    pub camera_heading: f32,
    pub camera_scale: f32,
    pub camera_offset: Vec3d,

    pub buffer_objects: Vec<GlObjectBuffers>, // all the subobjects, conditionally rendered based on the current tree selection
    pub buffer_textures: HashMap<TextureId, SrgbTexture2d>, // map of tex ids to actual textures
    pub buffer_shield: Option<GlBufferedShield>, // the shield, similar to the above
    pub buffer_insignias: Vec<GlBufferedInsignia>, // the insignias, similar to the above
    pub lollipops: Vec<GlLollipops>, // the current set of lollipops being being drawn, grouped by color, and recalculated with viewport_3d_dirty above
    pub arrowheads: Vec<GlArrowhead>, // The arrowheads to draw
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
            camera_pitch: Default::default(),
            camera_heading: Default::default(),
            camera_scale: Default::default(),
            camera_offset: Default::default(),
            buffer_objects: Default::default(),
            buffer_textures: Default::default(),
            buffer_shield: Default::default(),
            buffer_insignias: Default::default(),
            lollipops: Default::default(),
            arrowheads: Default::default(),
        }
    }

    fn tree_selectable_item(&mut self, ui: &mut Ui, name: &str, selection: TreeValue) {
        self.ui_state.tree_selectable_item(&self.model, ui, name, selection);
    }
}

pub const ERROR_RED: Color32 = Color32::from_rgb(255, 50, 50);
pub const WARNING_YELLOW: Color32 = Color32::from_rgb(255, 255, 0);
pub const LIGHT_ORANGE: Color32 = Color32::from_rgb(210, 150, 128);
pub const LIGHT_BLUE: Color32 = Color32::from_rgb(0xA0, 0xD8, 0xFF);

impl UiState {
    /// returns the RichText for a given tree value to be displayed, mostly for the purposes of coloring it specially
    fn tree_val_text(&self, model: &Model, tree_value: TreeValue, this_name: &str) -> RichText {
        let text = RichText::new(this_name);
        for error in &model.errors {
            if let Error::DuplicateSubobjectName(err_id) = *error {
                match tree_value {
                    TreeValue::SubObjects(SubObjectTreeValue::Header) => return text.color(ERROR_RED),
                    TreeValue::SubObjects(SubObjectTreeValue::SubObject(obj_id)) => {
                        if model.sub_objects[obj_id].name == model.sub_objects[err_id].name {
                            return text.color(ERROR_RED);
                        }
                    }
                    _ => (),
                }
            } else if let Some(error_tree_value) = TreeValue::from_error(*error) {
                if tree_value == error_tree_value || tree_value.is_ancestor_of(error_tree_value) {
                    return text.color(ERROR_RED);
                }
            }
        }

        for warning in &model.warnings {
            if let Warning::DuplicatePathName(warn_idx) = *warning {
                match tree_value {
                    TreeValue::Paths(PathTreeValue::Header) => return text.color(WARNING_YELLOW),
                    TreeValue::Paths(PathTreeValue::Path(path_idx)) => {
                        if model.paths[path_idx].name == model.paths[warn_idx].name {
                            return text.color(WARNING_YELLOW);
                        }
                    }
                    _ => (),
                }
            } else if let Some(warning_tree_value) = TreeValue::from_warning(*warning) {
                if tree_value == warning_tree_value || tree_value.is_ancestor_of(warning_tree_value) {
                    return text.color(WARNING_YELLOW);
                }
            }
        }

        if let TreeValue::SubObjects(SubObjectTreeValue::SubObject(selected_id)) = self.tree_view_selection {
            if let TreeValue::SubObjects(SubObjectTreeValue::SubObject(this_id)) = tree_value {
                for link in &model.sub_objects[this_id].name_links {
                    match *link {
                        pof::NameLink::DestroyedVersion(id) | pof::NameLink::DestroyedVersionOf(id) if id == selected_id => {
                            return text.color(Color32::LIGHT_RED)
                        }
                        pof::NameLink::LiveDebris(id) | pof::NameLink::LiveDebrisOf(id) if id == selected_id => return text.color(LIGHT_ORANGE),
                        pof::NameLink::DetailLevel(id, _) | pof::NameLink::DetailLevelOf(id, _) if id == selected_id => {
                            return text.color(LIGHT_BLUE)
                        }
                        _ => {}
                    }
                }
            }
        }

        text
    }

    fn tree_selectable_item(&mut self, model: &Model, ui: &mut Ui, name: &str, tree_value: TreeValue) {
        let text = self.tree_val_text(model, tree_value, name);
        if ui.selectable_value(&mut self.tree_view_selection, tree_value, text).clicked() {
            self.refresh_properties_panel(model);
            self.viewport_3d_dirty = true;

            info!("Switched to {}", self.tree_view_selection);

            // maybe update ast selected object
            if let TreeValue::SubObjects(SubObjectTreeValue::SubObject(id)) = self.tree_view_selection {
                self.last_selected_subobj = Some(id);
            } else if let TreeValue::SubObjects(SubObjectTreeValue::Header) | TreeValue::Header = self.tree_view_selection {
                self.last_selected_subobj = model.header.detail_levels.first().copied();
            }
        }
    }

    fn tree_collapsing_item(&mut self, model: &Model, ui: &mut Ui, name: &str, tree_value: TreeValue, body: impl FnOnce(&mut UiState, &mut Ui)) {
        let mut state = CollapsingState::load_with_default_open(ui.ctx(), Id::new(tree_value), false);
        if self.tree_view_toggle == Some(tree_value) {
            state.toggle(ui);
            self.tree_view_toggle = None;
        } else if self.tree_view_force_open.is_some() && tree_value.is_ancestor_of(self.tree_view_force_open.unwrap()) {
            state.set_open(true);
        }

        let text = self.tree_val_text(model, tree_value, name);

        state
            .show_header(ui, |ui| {
                let response = ui.selectable_label(tree_value == self.tree_view_selection, text);

                if response.double_clicked() {
                    self.tree_view_toggle = Some(tree_value);
                } else if response.clicked() {
                    self.tree_view_selection = tree_value;
                    self.refresh_properties_panel(model);
                    self.viewport_3d_dirty = true;

                    info!("Switched to {}", self.tree_view_selection);

                    // maybe update last selected object
                    if let TreeValue::SubObjects(SubObjectTreeValue::SubObject(id)) = self.tree_view_selection {
                        self.last_selected_subobj = Some(id);
                    } else if let TreeValue::SubObjects(SubObjectTreeValue::Header) | TreeValue::Header = self.tree_view_selection {
                        self.last_selected_subobj = model.header.detail_levels.first().copied();
                    }
                }
            })
            .body(|ui| body(self, ui));
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
                ui.menu_button("File", |ui| {
                    ui.style_mut().spacing.item_spacing.y = 1.0;

                    if ui.button("Open").clicked() {
                        self.start_loading_model(None);
                        ui.output().cursor_icon = egui::CursorIcon::Wait;
                    }

                    if ui
                        .add_enabled(self.model.errors.is_empty(), Button::new("Save"))
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

                    if ui.button("Global Import").on_hover_text("Deletes data from the existing model and replaces it with the target model's. \n\
                                                                Replaces mass, moment of inertia, weapon points, docking points, thrusters, glow points, \
                                                                special points, turrets (only exact base and gun object name matches are retained), \
                                                                paths, eye points, and insignia.").clicked() {

                        let model = crossbeam::thread::scope(|s| s.spawn(|_| PofToolsGui::load_model(None)).join().unwrap()).unwrap();

                        if let Ok(Some(model)) = model {
                            self.model.global_import(model);
                            self.tree_view_selection = TreeValue::Header;
                            self.ui_state.refresh_properties_panel(&self.model);
                            self.viewport_3d_dirty = true;
                        }
                    }

                    if ui.input().pointer.interact_pos().map_or(false, |pos| !ui.min_rect().expand(20.0).contains(pos)) {
                        ui.close_menu();
                    }
                });

                ui.separator();

                ui.menu_button(RichText::new(format!("Version: {}", self.model.version)).text_style(TextStyle::Button), |ui| {
                    let mut changed = false;
                    Version::for_each(|version| {
                        if version >= Version::V21_16 {
                            changed |= ui
                                .radio_value(&mut self.model.version, version, version.to_str())
                                .on_hover_text(version.documentation())
                                .changed();
                        }
                    });

                    // we only need to recheck verson-specific warnings, but since those are parameterized, there's no easy way to say
                    // 'those specific warnings but for all their parameters' so just do them all i guess
                    if changed {
                        self.model.recheck_warnings(pof::Set::All); // FIX
                        self.model.recheck_errors(pof::Set::All);
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
            .default_height(22.0)
            .height_range(22.0..=500.0)
            .show(ctx, |ui| {
                egui::ScrollArea::vertical()
                    .auto_shrink([false, false])
                    .min_scrolled_height(10.0)
                    .show(ui, |ui| {
                        let mut new_tree_val = None;
                        let mut first_warning = true;
                        for &error in &self.model.errors {
                            let str = match error {
                                Error::InvalidTurretGunSubobject(turret_num) => {
                                    format!("⊗ {} has an invalid gun object", self.model.sub_objects[self.model.turrets[turret_num].base_obj].name)
                                }
                                Error::TooManyDebrisObjects => {
                                    let mut num_debris = 0;
                                    for sobj in &self.model.sub_objects {
                                        if sobj.is_debris_model {
                                            num_debris += 1;
                                        }
                                    }
                                    format!("⊗ This model has too many debris objects ({}/{})", num_debris, pof::MAX_DEBRIS_OBJECTS)
                                }
                                Error::DetailObjWithParent(id) => {
                                    format!(
                                        "⊗ Detail {} object ({}) must be at the top of the hierarchy (no object parent)",
                                        self.model.header.detail_levels.iter().position(|detail_id| *detail_id == id).unwrap(),
                                        self.model.sub_objects[id].name,
                                    )
                                }
                                Error::DetailAndDebrisObj(id) => {
                                    format!(
                                        "⊗ Detail {} object ({}) cannot also be a debris object",
                                        self.model.header.detail_levels.iter().position(|detail_id| *detail_id == id).unwrap(),
                                        self.model.sub_objects[id].name,
                                    )
                                }
                                Error::TooManyVerts(id) => {
                                    format!(
                                        "⊗ Subobject {} has more than the {} vertices supported by the currently selected pof version",
                                        self.model.sub_objects[id].name,
                                        self.model.max_verts_norms_per_subobj(),
                                    )
                                }
                                Error::TooManyNorms(id) => {
                                    format!(
                                        "⊗ Subobject {} has more than the {} normals supported by the currently selected pof version",
                                        self.model.sub_objects[id].name,
                                        self.model.max_verts_norms_per_subobj(),
                                    )
                                }
                                Error::UnnamedSubObject(id) => {
                                    format!("⊗ Subobject id {:?} requires a name", id)
                                }
                                Error::DuplicateSubobjectName(id) => {
                                    format!("⊗ More than one subobject shares the name '{}'", self.model.sub_objects[id].name)
                                }
                            };

                            let text = RichText::new(str).text_style(TextStyle::Button).color(ERROR_RED);
                            if first_warning {
                                ui.horizontal(|ui| {
                                    if let Some(tree_val) = TreeValue::from_error(error) {
                                        if ui.selectable_label(false, text).clicked() {
                                            new_tree_val = Some(tree_val);
                                        }
                                    } else {
                                        ui.label(text);
                                    }
                                    ui.with_layout(egui::Layout::right_to_left(egui::Align::Min), |ui| {
                                        if !self.model.errors.is_empty() {
                                            ui.add(Label::new(
                                                RichText::new(format!("{} ⊗", self.model.errors.len()))
                                                    .text_style(TextStyle::Button)
                                                    .color(ERROR_RED),
                                            ));
                                        }

                                        if !self.model.warnings.is_empty() {
                                            ui.add(Label::new(
                                                RichText::new(format!("{} ⚠", self.model.warnings.len()))
                                                    .text_style(TextStyle::Button)
                                                    .color(WARNING_YELLOW),
                                            ));
                                        }
                                    });
                                });
                                first_warning = false;
                            } else if let Some(tree_val) = TreeValue::from_error(error) {
                                if ui.selectable_label(false, text).clicked() {
                                    new_tree_val = Some(tree_val);
                                }
                            } else {
                                ui.label(text);
                            }
                        }

                        for &warning in &self.model.warnings {
                            let str = match warning {
                                Warning::InvertedBBox(id_opt) => {
                                    format!("⚠ {}'s bounding box is inverted", id_opt.map_or("The header", |id| &self.model.sub_objects[id].name))
                                }
                                Warning::DockingBayWithoutPath(bay_num) => {
                                    format!(
                                        "⚠ Docking bay {} cannot be used by ships without a path",
                                        self.model.docking_bays[bay_num].get_name().unwrap_or(&(bay_num + 1).to_string())
                                    )
                                }
                                Warning::ThrusterPropertiesInvalidVersion(idx) => {
                                    format!("⚠ Thruster bank {} has properties, which the currently selected version does not support", idx + 1)
                                }
                                Warning::WeaponOffsetInvalidVersion { primary, bank, point } => {
                                    format!(
                                        "⚠ {} bank {}, point {}, has an external angle offset, which the currently selected version does not support",
                                        if primary { "Primary" } else { "Secondary" },
                                        bank + 1,
                                        point + 1
                                    )
                                }
                                Warning::SubObjectTranslationInvalidVersion(id) => {
                                    format!(
                                        "⚠ Subobject {} has a translation axis defined, which the currently selected version does not support",
                                        self.model.sub_objects[id].name
                                    )
                                }
                                Warning::UntexturedPolygons => {
                                    format!(
                                        "⚠ This model has untextured polygons (A texture slot has been added which corresponds to these polygons)"
                                    )
                                }
                                Warning::TooManyEyePoints => {
                                    format!("⚠ You cannot have more than {} eye points.", pof::MAX_EYES)
                                }
                                Warning::TooManyTextures => {
                                    format!("⚠ You cannot have more than {} textures.", pof::MAX_TEXTURES)
                                }
                                Warning::TooFewTurretFirePoints(idx) => {
                                    format!("⚠ {} must have at least 1 fire point.", self.model.sub_objects[self.model.turrets[idx].base_obj].name)
                                }
                                Warning::TooManyTurretFirePoints(idx) => {
                                    format!(
                                        "⚠ {} must have at most {} fire points.",
                                        self.model.sub_objects[self.model.turrets[idx].base_obj].name,
                                        pof::MAX_TURRET_POINTS
                                    )
                                }
                                Warning::RadiusTooSmall(id_opt) => {
                                    format!(
                                        "⚠ {}'s radius does not encompass all of its geometry",
                                        id_opt.map_or("The header", |id| &self.model.sub_objects[id].name)
                                    )
                                }
                                Warning::BBoxTooSmall(id_opt) => {
                                    format!(
                                        "⚠ {}'s bounding box does not encompass all of its geometry",
                                        id_opt.map_or("The header", |id| &self.model.sub_objects[id].name)
                                    )
                                }
                                Warning::DuplicatePathName(path_idx) => {
                                    format!("⚠ More than one path shares the name '{}'", self.model.paths[path_idx].name)
                                }
                                Warning::DuplicateDetailLevel(id) => {
                                    format!("⚠ Subobject '{}' belongs to more than one detail level", self.model.sub_objects[id].name)
                                }
                                Warning::PathNameTooLong(_)
                                | Warning::SubObjectNameTooLong(_)
                                | Warning::SpecialPointNameTooLong(_)
                                | Warning::DockingBayNameTooLong(_) => {
                                    let field = match warning {
                                        Warning::PathNameTooLong(idx) => {
                                            format!("Path name '{}'", self.model.paths[idx].name)
                                        }
                                        Warning::SubObjectNameTooLong(id) => {
                                            format!("Subobject name '{}'", self.model.sub_objects[id].name)
                                        }
                                        Warning::SpecialPointNameTooLong(idx) => {
                                            format!("Special point name '{}'", self.model.special_points[idx].name)
                                        }
                                        Warning::DockingBayNameTooLong(idx) => {
                                            format!("Docking bay name '{}'", self.model.special_points[idx].name)
                                        }
                                        _ => unreachable!(),
                                    };
                                    format!("⚠ {} is too long (max {} bytes)", field, pof::MAX_NAME_LEN)
                                }
                                Warning::GlowBankPropertiesTooLong(_)
                                | Warning::ThrusterPropertiesTooLong(_)
                                | Warning::SubObjectPropertiesTooLong(_)
                                | Warning::DockingBayPropertiesTooLong(_)
                                | Warning::SpecialPointPropertiesTooLong(_) => {
                                    let field = match warning {
                                        Warning::GlowBankPropertiesTooLong(idx) => {
                                            format!(
                                                "Glow bank {} ({}) properties",
                                                idx,
                                                pof::properties_get_field(&self.model.glow_banks[idx].properties, "$glow_texture")
                                                    .unwrap_or_default()
                                            )
                                        }
                                        Warning::ThrusterPropertiesTooLong(idx) => {
                                            format!("Thruster bank {} properties", idx + 1)
                                        }
                                        Warning::SubObjectPropertiesTooLong(id) => {
                                            format!("Subobject {} properties", self.model.sub_objects[id].name)
                                        }
                                        Warning::DockingBayPropertiesTooLong(idx) => {
                                            format!("Docking bay {} properties", idx + 1)
                                        }
                                        Warning::SpecialPointPropertiesTooLong(idx) => {
                                            format!("Special point {} properties", self.model.special_points[idx].name)
                                        }
                                        _ => unreachable!(),
                                    };
                                    format!("⚠ {} is too long (max {} bytes)", field, pof::MAX_PROPERTIES_LEN)
                                }
                            };

                            let text = RichText::new(str).text_style(TextStyle::Button).color(WARNING_YELLOW);
                            if first_warning {
                                ui.horizontal(|ui| {
                                    if let Some(tree_val) = TreeValue::from_warning(warning) {
                                        if ui.selectable_label(false, text).clicked() {
                                            new_tree_val = Some(tree_val);
                                        }
                                    } else {
                                        ui.label(text);
                                    }
                                    ui.with_layout(egui::Layout::right_to_left(egui::Align::Min), |ui| {
                                        if !self.model.errors.is_empty() {
                                            ui.add(Label::new(
                                                RichText::new(format!("{} ⊗", self.model.errors.len()))
                                                    .text_style(TextStyle::Button)
                                                    .color(ERROR_RED),
                                            ));
                                        }

                                        if !self.model.warnings.is_empty() {
                                            ui.add(Label::new(
                                                RichText::new(format!("{} ⚠", self.model.warnings.len()))
                                                    .text_style(TextStyle::Button)
                                                    .color(WARNING_YELLOW),
                                            ));
                                        }
                                    });
                                });
                                first_warning = false;
                            } else if let Some(tree_val) = TreeValue::from_warning(warning) {
                                if ui.selectable_label(false, text).clicked() {
                                    new_tree_val = Some(tree_val);
                                }
                            } else {
                                ui.label(text);
                            }
                        }

                        if let Some(tree_val) = new_tree_val {
                            self.tree_view_selection = tree_val;
                            self.tree_view_force_open = Some(tree_val);
                            self.ui_state.refresh_properties_panel(&self.model);
                            self.ui_state.viewport_3d_dirty = true;
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
                    self.tree_selectable_item(ui, "Header", TreeValue::Header);

                    let num_subobjs = self.model.sub_objects.len();
                    let name = format!("SubObjects{}", if num_subobjs > 0 { format!(", {}", num_subobjs) } else { String::new() });
                    self.ui_state
                        .tree_collapsing_item(&self.model, ui, &name, TreeValue::SubObjects(SubObjectTreeValue::Header), |ui_state, ui| {
                            fn make_subobject_child_list(ui_state: &mut UiState, model: &Model, obj: &SubObject, ui: &mut Ui) {
                                let selection = TreeValue::SubObjects(SubObjectTreeValue::SubObject(obj.obj_id));
                                if obj.children().next() == None {
                                    ui_state.tree_selectable_item(model, ui, &obj.name, selection);
                                } else {
                                    ui_state.tree_collapsing_item(model, ui, &obj.name, selection, |ui_state, ui| {
                                        for &i in obj.children() {
                                            make_subobject_child_list(ui_state, model, &model.sub_objects[i], ui)
                                        }
                                    });
                                }
                            }

                            for object in &self.model.sub_objects {
                                if object.parent().is_none() {
                                    make_subobject_child_list(ui_state, &self.model, object, ui);
                                }
                            }
                        });

                    let num_textures = self.model.textures.len();
                    let name = format!(
                        "Textures{}",
                        if num_textures > 0 {
                            format!(", {}", num_textures)
                        } else {
                            String::new()
                        }
                    );
                    self.ui_state
                        .tree_collapsing_item(&self.model, ui, &name, TreeValue::Textures(TextureTreeValue::Header), |ui_state, ui| {
                            for (i, tex) in self.model.textures.iter().enumerate() {
                                ui_state.tree_selectable_item(
                                    &self.model,
                                    ui,
                                    tex,
                                    TreeValue::Textures(TextureTreeValue::Texture(TextureId(i as u32))),
                                );
                            }
                        });

                    let num_banks = self.model.thruster_banks.len();
                    let name = format!("Thrusters{}", if num_banks > 0 { format!(", {}", num_banks) } else { String::new() });
                    self.ui_state
                        .tree_collapsing_item(&self.model, ui, &name, TreeValue::Thrusters(ThrusterTreeValue::Header), |ui_state, ui| {
                            for (i, thruster_bank) in self.model.thruster_banks.iter().enumerate() {
                                ui_state.tree_collapsing_item(
                                    &self.model,
                                    ui,
                                    &format!("Bank {}, {}", i + 1, thruster_bank.glows.len()),
                                    TreeValue::Thrusters(ThrusterTreeValue::Bank(i)),
                                    |ui_state, ui| {
                                        for j in 0..thruster_bank.glows.len() {
                                            ui_state.tree_selectable_item(
                                                &self.model,
                                                ui,
                                                &format!("Point {}", j + 1),
                                                TreeValue::Thrusters(ThrusterTreeValue::BankPoint(i, j)),
                                            );
                                        }
                                    },
                                );
                            }
                        });

                    let num_banks = self.model.primary_weps.len() + self.model.secondary_weps.len();
                    let name = format!("Weapons{}", if num_banks > 0 { format!(", {}", num_banks) } else { String::new() });
                    self.ui_state
                        .tree_collapsing_item(&self.model, ui, &name, TreeValue::Weapons(WeaponTreeValue::Header), |ui_state, ui| {
                            let num_banks = self.model.primary_weps.len();
                            let name = format!("Primary Weapons{}", if num_banks > 0 { format!(", {}", num_banks) } else { String::new() });
                            ui_state.tree_collapsing_item(&self.model, ui, &name, TreeValue::Weapons(WeaponTreeValue::PriHeader), |ui_state, ui| {
                                for (i, primary_bank) in self.model.primary_weps.iter().enumerate() {
                                    ui_state.tree_collapsing_item(
                                        &self.model,
                                        ui,
                                        &format!("Bank {}, {}", i + 1, primary_bank.len()),
                                        TreeValue::Weapons(WeaponTreeValue::PriBank(i)),
                                        |ui_state, ui| {
                                            for j in 0..primary_bank.len() {
                                                ui_state.tree_selectable_item(
                                                    &self.model,
                                                    ui,
                                                    &format!("Point {}", j + 1),
                                                    TreeValue::Weapons(WeaponTreeValue::PriBankPoint(i, j)),
                                                );
                                            }
                                        },
                                    );
                                }
                            });

                            let num_banks = self.model.secondary_weps.len();
                            let name = format!("Secondary Weapons{}", if num_banks > 0 { format!(", {}", num_banks) } else { String::new() });
                            ui_state.tree_collapsing_item(&self.model, ui, &name, TreeValue::Weapons(WeaponTreeValue::SecHeader), |ui_state, ui| {
                                for (i, secondary_bank) in self.model.secondary_weps.iter().enumerate() {
                                    ui_state.tree_collapsing_item(
                                        &self.model,
                                        ui,
                                        &format!("Bank {}, {}", i + 1, secondary_bank.len()),
                                        TreeValue::Weapons(WeaponTreeValue::SecBank(i)),
                                        |ui_state, ui| {
                                            for j in 0..secondary_bank.len() {
                                                ui_state.tree_selectable_item(
                                                    &self.model,
                                                    ui,
                                                    &format!("Point {}", j + 1),
                                                    TreeValue::Weapons(WeaponTreeValue::SecBankPoint(i, j)),
                                                );
                                            }
                                        },
                                    );
                                }
                            });
                        });

                    let num_bays = self.model.docking_bays.len();
                    let name = format!("Docking Bays{}", if num_bays > 0 { format!(", {}", num_bays) } else { String::new() });
                    self.ui_state
                        .tree_collapsing_item(&self.model, ui, &name, TreeValue::DockingBays(DockingTreeValue::Header), |ui_state, ui| {
                            for (i, docking_bay) in self.model.docking_bays.iter().enumerate() {
                                ui_state.tree_selectable_item(
                                    &self.model,
                                    ui,
                                    docking_bay.get_name().unwrap_or(&format!("Bay {}", i + 1)),
                                    TreeValue::DockingBays(DockingTreeValue::Bay(i)),
                                );
                            }
                        });

                    let num_glow_banks = self.model.glow_banks.len();
                    let name = format!(
                        "Glow Points{}",
                        if num_glow_banks > 0 {
                            format!(", {}", num_glow_banks)
                        } else {
                            String::new()
                        }
                    );
                    self.ui_state
                        .tree_collapsing_item(&self.model, ui, &name, TreeValue::Glows(GlowTreeValue::Header), |ui_state, ui| {
                            for (i, glow_bank) in self.model.glow_banks.iter().enumerate() {
                                ui_state.tree_collapsing_item(
                                    &self.model,
                                    ui,
                                    &format!(
                                        "Bank {}{}, {}",
                                        i + 1,
                                        pof::properties_get_field(&self.model.glow_banks[i].properties, "$glow_texture")
                                            .map_or(String::new(), |tex| format!(" ({})", tex)),
                                        glow_bank.glow_points.len()
                                    ),
                                    TreeValue::Glows(GlowTreeValue::Bank(i)),
                                    |ui_state, ui| {
                                        for j in 0..glow_bank.glow_points.len() {
                                            ui_state.tree_selectable_item(
                                                &self.model,
                                                ui,
                                                &format!("Point {}", j + 1),
                                                TreeValue::Glows(GlowTreeValue::BankPoint(i, j)),
                                            );
                                        }
                                    },
                                );
                            }
                        });

                    let num_points = self.model.special_points.len();
                    let name = format!("Special Points{}", if num_points > 0 { format!(", {}", num_points) } else { String::new() });
                    self.ui_state.tree_collapsing_item(
                        &self.model,
                        ui,
                        &name,
                        TreeValue::SpecialPoints(SpecialPointTreeValue::Header),
                        |ui_state, ui| {
                            for (i, special_point) in self.model.special_points.iter().enumerate() {
                                ui_state.tree_selectable_item(
                                    &self.model,
                                    ui,
                                    &special_point.name,
                                    TreeValue::SpecialPoints(SpecialPointTreeValue::Point(i)),
                                );
                            }
                        },
                    );

                    let num_turrets = self.model.turrets.len();
                    let name = format!("Turrets{}", if num_turrets > 0 { format!(", {}", num_turrets) } else { String::new() });
                    self.ui_state
                        .tree_collapsing_item(&self.model, ui, &name, TreeValue::Turrets(TurretTreeValue::Header), |ui_state, ui| {
                            for (i, turret) in self.model.turrets.iter().enumerate() {
                                ui_state.tree_collapsing_item(
                                    &self.model,
                                    ui,
                                    &format!("{}, {}", self.model.sub_objects[turret.base_obj].name, turret.fire_points.len()),
                                    TreeValue::Turrets(TurretTreeValue::Turret(i)),
                                    |ui_state, ui| {
                                        for j in 0..turret.fire_points.len() {
                                            ui_state.tree_selectable_item(
                                                &self.model,
                                                ui,
                                                &format!("Fire Point {}", j + 1),
                                                TreeValue::Turrets(TurretTreeValue::TurretPoint(i, j)),
                                            );
                                        }
                                    },
                                );
                            }
                        });

                    let num_paths = self.model.paths.len();
                    let name = format!("Paths{}", if num_paths > 0 { format!(", {}", num_paths) } else { String::new() });
                    self.ui_state
                        .tree_collapsing_item(&self.model, ui, &name, TreeValue::Paths(PathTreeValue::Header), |ui_state, ui| {
                            for (i, path) in self.model.paths.iter().enumerate() {
                                ui_state.tree_collapsing_item(
                                    &self.model,
                                    ui,
                                    &format!("{}, {}", path.name, path.points.len()),
                                    TreeValue::Paths(PathTreeValue::Path(i)),
                                    |ui_state, ui| {
                                        for j in 0..path.points.len() {
                                            ui_state.tree_selectable_item(
                                                &self.model,
                                                ui,
                                                &format!("Path Point {}", j + 1),
                                                TreeValue::Paths(PathTreeValue::PathPoint(i, j)),
                                            );
                                        }
                                    },
                                );
                            }
                        });

                    let num_insigs = self.model.insignias.len();
                    let name = format!("Insignias{}", if num_insigs > 0 { format!(", {}", num_insigs) } else { String::new() });
                    self.ui_state
                        .tree_collapsing_item(&self.model, ui, &name, TreeValue::Insignia(InsigniaTreeValue::Header), |ui_state, ui| {
                            for (i, _) in self.model.insignias.iter().enumerate() {
                                ui_state.tree_selectable_item(
                                    &self.model,
                                    ui,
                                    &format!("Insignia {}", i + 1),
                                    TreeValue::Insignia(InsigniaTreeValue::Insignia(i)),
                                );
                            }
                        });

                    self.ui_state.tree_selectable_item(
                        &self.model,
                        ui,
                        if self.model.shield_data.is_some() { "Shield" } else { "(No Shield)" },
                        TreeValue::Shield,
                    );

                    let num_eyes = self.model.eye_points.len();
                    let name = format!("Eye Points{}", if num_eyes > 0 { format!(", {}", num_eyes) } else { String::new() });
                    self.ui_state
                        .tree_collapsing_item(&self.model, ui, &name, TreeValue::EyePoints(EyeTreeValue::Header), |ui_state, ui| {
                            for (i, eye) in self.model.eye_points.iter().enumerate() {
                                ui_state.tree_selectable_item(
                                    &self.model,
                                    ui,
                                    &format!("{} {}", self.model.sub_objects[eye.attached_subobj].name, i + 1),
                                    TreeValue::EyePoints(EyeTreeValue::EyePoint(i)),
                                );
                            }
                        });

                    self.ui_state
                        .tree_selectable_item(&self.model, ui, "Visual Center", TreeValue::VisualCenter);

                    self.ui_state.tree_selectable_item(&self.model, ui, "Comments", TreeValue::Comments);
                });
            });

        // all the tree values needing opening should have been opened by now
        self.tree_view_force_open = None;

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
                    ui.add_space(3.0);

                    self.do_properties_panel(ui, ctx, display);
                });
            });
    }
}
