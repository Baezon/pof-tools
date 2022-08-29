use egui::{CollapsingHeader, Color32, Label, RichText};
use glium::{
    texture::{RawImage2d, SrgbTexture2d},
    Display,
};
use pof::{BoundingBox, Model, SubObject, TextureId, Vec3d, Version};
use std::{
    collections::{BTreeSet, HashMap},
    sync::mpsc::Receiver,
};

use eframe::egui::{self, Button, TextStyle, Ui};
use pof::ObjectId;

use crate::{ui_properties_panel::PropertiesPanel, GlBufferedInsignia, GlBufferedShield, GlLollipops, GlArrowhead, GlObjectBuffers, POF_TOOLS_VERSION};

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
    pub fn point(point: Option<usize>) -> Self {
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
    pub fn point(point: Option<usize>) -> Self {
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
    pub fn bay(bay: Option<usize>) -> Self {
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

use Set::*;

pub enum Set<T> {
    All,
    One(T),
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum Error {
    InvalidTurretGunSubobject(usize), // turret index
    TooManyDebrisObjects,
    DetailObjWithParent(ObjectId),
    DetailAndDebrisObj(ObjectId),
    TooManyVerts(ObjectId),
    TooManyNorms(ObjectId),
    // all turret base/gun objects must be disjoint!
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Copy, Clone)]
pub(crate) enum Warning {
    RadiusTooSmall(Option<ObjectId>),
    BBoxTooSmall(Option<ObjectId>),
    DockingBayWithoutPath(usize),
    ThrusterPropertiesInvalidVersion(usize),
    WeaponOffsetInvalidVersion(WeaponSelection),
    InvertedBBox(Option<ObjectId>),
    UntexturedPolygons,
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
    pub warnings: BTreeSet<Warning>,
    pub errors: BTreeSet<Error>,

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
            arrowheads: Default::default(),
        }
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
                        PofToolsGui::recheck_warnings(&mut self.warnings, &self.model, All);
                        PofToolsGui::recheck_errors(&mut self.errors, &self.model, All);
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
                                Error::DetailObjWithParent(id) => {
                                    ui.add(Label::new(
                                        RichText::new(format!(
                                            "⊗ Detail {} object ({}) must be at the top of the heirarchy (no object parent)",
                                            self.model.header.detail_levels.iter().position(|detail_id| *detail_id == id).unwrap(),
                                            self.model.sub_objects[id].name,
                                        ))
                                        .text_style(TextStyle::Button)
                                        .color(Color32::RED),
                                    ));
                                }
                                Error::DetailAndDebrisObj(id) => {
                                    ui.add(Label::new(
                                        RichText::new(format!(
                                            "⊗ Detail {} object ({}) cannot also be a debris object",
                                            self.model.header.detail_levels.iter().position(|detail_id| *detail_id == id).unwrap(),
                                            self.model.sub_objects[id].name,
                                        ))
                                        .text_style(TextStyle::Button)
                                        .color(Color32::RED),
                                    ));
                                }
                                Error::TooManyVerts(id) => {
                                    ui.add(Label::new(
                                        RichText::new(format!(
                                            "⊗ Subobject {} has more than the {} vertices supported by the currently selected pof version",
                                            self.model.sub_objects[id].name,
                                            self.model.max_verts_norms_per_subobj(),
                                        ))
                                        .text_style(TextStyle::Button)
                                        .color(Color32::RED),
                                    ));
                                }
                                Error::TooManyNorms(id) => {
                                    ui.add(Label::new(
                                        RichText::new(format!(
                                            "⊗ Subobject {} has more than the {} normals supported by the currently selected pof version",
                                            self.model.sub_objects[id].name,
                                            self.model.max_verts_norms_per_subobj(),
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
                                Warning::InvertedBBox(id_opt) => {
                                    let str = format!(
                                        "⚠ {}'s bounding box is inverted",
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
                                Warning::UntexturedPolygons => {
                                    let str = format!(
                                        "⚠ This model has untextured polygons (A texture slot has been added which corresponds to these polygons)"
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

                    let num_subobjs = self.model.sub_objects.len();
                    let name = format!("SubObjects{}", if num_subobjs > 0 { format!(", {}", num_subobjs) } else { String::new() });
                    self.ui_state.tree_collapsing_item(
                        &self.model,
                        ui,
                        &name,
                        TreeSelection::SubObjects(SubObjectSelection::Header),
                        |ui_state, ui| {
                            fn make_subobject_child_list(ui_state: &mut UiState, model: &Model, obj: &SubObject, ui: &mut Ui) {
                                let selection = TreeSelection::SubObjects(SubObjectSelection::SubObject(obj.obj_id));
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
                        },
                    );

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
                        .tree_collapsing_item(&self.model, ui, &name, TreeSelection::Textures(TextureSelection::Header), |ui_state, ui| {
                            for (i, tex) in self.model.textures.iter().enumerate() {
                                ui_state.tree_selectable_item(
                                    &self.model,
                                    ui,
                                    tex,
                                    TreeSelection::Textures(TextureSelection::Texture(TextureId(i as u32))),
                                );
                            }
                        });

                    let num_banks = self.model.thruster_banks.len();
                    let name = format!("Thrusters{}", if num_banks > 0 { format!(", {}", num_banks) } else { String::new() });
                    self.ui_state.tree_collapsing_item(
                        &self.model,
                        ui,
                        &name,
                        TreeSelection::Thrusters(ThrusterSelection::Header),
                        |ui_state, ui| {
                            for (i, thruster_bank) in self.model.thruster_banks.iter().enumerate() {
                                ui_state.tree_collapsing_item(
                                    &self.model,
                                    ui,
                                    &format!("Bank {}, {}", i + 1, thruster_bank.glows.len()),
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

                    let num_banks = self.model.primary_weps.len() + self.model.secondary_weps.len();
                    let name = format!("Weapons{}", if num_banks > 0 { format!(", {}", num_banks) } else { String::new() });
                    self.ui_state
                        .tree_collapsing_item(&self.model, ui, &name, TreeSelection::Weapons(WeaponSelection::Header), |ui_state, ui| {
                            let num_banks = self.model.primary_weps.len();
                            let name = format!("Primary Weapons{}", if num_banks > 0 { format!(", {}", num_banks) } else { String::new() });
                            ui_state.tree_collapsing_item(
                                &self.model,
                                ui,
                                &name,
                                TreeSelection::Weapons(WeaponSelection::PriHeader),
                                |ui_state, ui| {
                                    for (i, primary_bank) in self.model.primary_weps.iter().enumerate() {
                                        ui_state.tree_collapsing_item(
                                            &self.model,
                                            ui,
                                            &format!("Bank {}, {}", i + 1, primary_bank.len()),
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

                            let num_banks = self.model.secondary_weps.len();
                            let name = format!("Secondary Weapons{}", if num_banks > 0 { format!(", {}", num_banks) } else { String::new() });
                            ui_state.tree_collapsing_item(
                                &self.model,
                                ui,
                                &name,
                                TreeSelection::Weapons(WeaponSelection::SecHeader),
                                |ui_state, ui| {
                                    for (i, secondary_bank) in self.model.secondary_weps.iter().enumerate() {
                                        ui_state.tree_collapsing_item(
                                            &self.model,
                                            ui,
                                            &format!("Bank {}, {}", i + 1, secondary_bank.len()),
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
                        });

                    let num_bays = self.model.docking_bays.len();
                    let name = format!("Docking Bays{}", if num_bays > 0 { format!(", {}", num_bays) } else { String::new() });
                    self.ui_state.tree_collapsing_item(
                        &self.model,
                        ui,
                        &name,
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
                        .tree_collapsing_item(&self.model, ui, &name, TreeSelection::Glows(GlowSelection::Header), |ui_state, ui| {
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
                        });

                    let num_points = self.model.special_points.len();
                    let name = format!("Special Points{}", if num_points > 0 { format!(", {}", num_points) } else { String::new() });
                    self.ui_state.tree_collapsing_item(
                        &self.model,
                        ui,
                        &name,
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

                    let num_turrets = self.model.turrets.len();
                    let name = format!("Turrets{}", if num_turrets > 0 { format!(", {}", num_turrets) } else { String::new() });
                    self.ui_state
                        .tree_collapsing_item(&self.model, ui, &name, TreeSelection::Turrets(TurretSelection::Header), |ui_state, ui| {
                            for (i, turret) in self.model.turrets.iter().enumerate() {
                                ui_state.tree_collapsing_item(
                                    &self.model,
                                    ui,
                                    &format!("{}, {}", self.model.sub_objects[turret.base_obj].name, turret.fire_points.len()),
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
                        });

                    let num_paths = self.model.paths.len();
                    let name = format!("Paths{}", if num_paths > 0 { format!(", {}", num_paths) } else { String::new() });
                    self.ui_state
                        .tree_collapsing_item(&self.model, ui, &name, TreeSelection::Paths(PathSelection::Header), |ui_state, ui| {
                            for (i, path) in self.model.paths.iter().enumerate() {
                                ui_state.tree_collapsing_item(
                                    &self.model,
                                    ui,
                                    &format!("{}, {}", path.name, path.points.len()),
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

                    let num_insigs = self.model.insignias.len();
                    let name = format!("Insignias{}", if num_insigs > 0 { format!(", {}", num_insigs) } else { String::new() });
                    self.ui_state
                        .tree_collapsing_item(&self.model, ui, &name, TreeSelection::Insignia(InsigniaSelection::Header), |ui_state, ui| {
                            for (i, _) in self.model.insignias.iter().enumerate() {
                                ui_state.tree_selectable_item(
                                    &self.model,
                                    ui,
                                    &format!("Insignia {}", i + 1),
                                    TreeSelection::Insignia(InsigniaSelection::Insignia(i)),
                                );
                            }
                        });

                    self.ui_state.tree_selectable_item(
                        &self.model,
                        ui,
                        if self.model.shield_data.is_some() { "Shield" } else { "(No Shield)" },
                        TreeSelection::Shield,
                    );

                    let num_eyes = self.model.eye_points.len();
                    let name = format!("Eye Points{}", if num_eyes > 0 { format!(", {}", num_eyes) } else { String::new() });
                    self.ui_state
                        .tree_collapsing_item(&self.model, ui, &name, TreeSelection::EyePoints(EyeSelection::Header), |ui_state, ui| {
                            for (i, eye) in self.model.eye_points.iter().enumerate() {
                                ui_state.tree_selectable_item(
                                    &self.model,
                                    ui,
                                    &format!("{} {}", self.model.sub_objects[eye.attached_subobj].name, i + 1),
                                    TreeSelection::EyePoints(EyeSelection::EyePoint(i)),
                                );
                            }
                        });

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
                    ui.add_space(3.0);

                    self.do_properties_panel(ui, ctx, display);
                });
            });
    }

    // rechecks just one or all of the warnings on the model
    pub fn recheck_errors(errors: &mut BTreeSet<Error>, model: &Model, error_to_check: Set<Error>) {
        if let One(error) = error_to_check {
            let failed_check = match error {
                Error::InvalidTurretGunSubobject(turret) => PofToolsGui::turret_gun_subobj_not_valid(model, turret),
                Error::TooManyDebrisObjects => model.num_debris_objects() > pof::MAX_DEBRIS_OBJECTS,
                Error::DetailAndDebrisObj(id) => model.header.detail_levels.contains(&id) && model.sub_objects[id].is_debris_model,
                Error::DetailObjWithParent(id) => model.header.detail_levels.contains(&id) && model.sub_objects[id].parent().is_some(),
                Error::TooManyVerts(id) => model.sub_objects[id].bsp_data.verts.len() > model.max_verts_norms_per_subobj(),
                Error::TooManyNorms(id) => model.sub_objects[id].bsp_data.norms.len() > model.max_verts_norms_per_subobj(),
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

            for &id in &model.header.detail_levels {
                let subobj = &model.sub_objects[id];
                if subobj.parent().is_some() {
                    errors.insert(Error::DetailObjWithParent(id));
                }
                if subobj.is_debris_model {
                    errors.insert(Error::DetailAndDebrisObj(id));
                }
            }

            for subobj in &model.sub_objects {
                if subobj.bsp_data.verts.len() > model.max_verts_norms_per_subobj() {
                    errors.insert(Error::TooManyVerts(subobj.obj_id));
                }

                if subobj.bsp_data.norms.len() > model.max_verts_norms_per_subobj() {
                    errors.insert(Error::TooManyNorms(subobj.obj_id));
                }
            }
        }
    }

    fn turret_gun_subobj_not_valid(model: &Model, turret_num: usize) -> bool {
        let turret = &model.turrets[turret_num];
        if turret.base_obj == turret.gun_obj {
            return false;
        }

        for &child_id in model.sub_objects[turret.base_obj].children() {
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
                Warning::InvertedBBox(id_opt) => {
                    if let Some(id) = id_opt {
                        model.sub_objects[id].bbox.is_inverted()
                    } else {
                        model.header.bbox.is_inverted()
                    }
                }
                Warning::UntexturedPolygons => model.untextured_idx.is_some(),
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

            if model.header.bbox.is_inverted() && model.header.bbox != BoundingBox::EMPTY {
                warnings.insert(Warning::InvertedBBox(None));
            }

            for subobj in &model.sub_objects {
                if PofToolsGui::bbox_test_failed(model, Some(subobj.obj_id)) {
                    warnings.insert(Warning::BBoxTooSmall(Some(subobj.obj_id)));
                }

                if subobj.bbox.is_inverted() && subobj.bbox != BoundingBox::EMPTY {
                    warnings.insert(Warning::InvertedBBox(Some(subobj.obj_id)));
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

            if model.untextured_idx.is_some() {
                warnings.insert(Warning::UntexturedPolygons);
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

                let offset = model.get_total_subobj_offset(subobj.obj_id);
                for vert in &subobj.bsp_data.verts {
                    if !model.header.bbox.contains(offset + *vert) {
                        return true;
                    }
                }
            }
        }

        false
    }
}
