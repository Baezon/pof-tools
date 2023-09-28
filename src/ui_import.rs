use egui::{collapsing_header::CollapsingState, Button, Color32, Id, Response, RichText, TextEdit, TextStyle, Ui, WidgetText};
use pof::{properties_delete_field, ObjectId, SubObject, TextureId};

use crate::{
    start_loading_import_model,
    ui::{
        DockingTreeValue, EyeTreeValue, GlowTreeValue, InsigniaTreeValue, PathTreeValue, PofToolsGui, SpecialPointTreeValue, SubObjectTreeValue,
        ThrusterTreeValue, TreeValue, TurretTreeValue, UiState, WeaponTreeValue, ERROR_RED, WARNING_YELLOW,
    },
    LoadingThread, Model,
};

use std::{
    collections::{hash_map::Entry, BTreeSet, HashMap},
    path::PathBuf,
};

#[derive(PartialEq)]
pub enum ImportType {
    Add,
    //ClearAndAdd,
    MatchAndReplace,
}

enum SelectionType {
    Total,
    Partial,
    None,
}

#[derive(Default)]
struct ImportOptions {
    auto_select_subobj_children: bool,
    auto_select_paths: bool,
    auto_select_turrets: bool,
}

/// The state associated to the GUI import window
pub struct ImportWindow {
    /// whether its open
    pub open: bool,
    /// what type of import has been selected (add or match-and-replace)
    pub import_type: ImportType,
    /// the model to be imported (if one has been selected)
    pub model: Option<Box<Model>>,
    /// the path to the model to be imported
    pub model_path: PathBuf,
    /// the thread handling loading of the modle to be imported
    pub import_model_loading_thread: LoadingThread,
    /// the set of tree values corresponding to the individual data structures to import
    pub import_selection: BTreeSet<TreeValue>,
    /// various options concerning selection of items in the GUI
    import_options: ImportOptions,
}
impl Default for ImportWindow {
    fn default() -> Self {
        Self {
            open: Default::default(),
            import_type: ImportType::Add,
            model: Default::default(),
            model_path: PathBuf::new(),
            import_model_loading_thread: Default::default(),
            import_selection: BTreeSet::new(),
            import_options: ImportOptions {
                auto_select_subobj_children: true,
                auto_select_paths: true,
                auto_select_turrets: true,
            },
        }
    }
}

impl UiState {
    pub fn show_import_window(&mut self, model: &Model, ctx: &egui::Context) -> bool {
        let window = egui::Window::new("Import")
            .collapsible(false)
            .resizable(true)
            .open(&mut self.import_window.open)
            .vscroll(true)
            .default_pos([100.0, 100.0]);

        let mut ret = false;

        window.show(ctx, |ui| {
            let selection = &self.import_window.import_selection;
            let num_subobjects_selected = selection.iter().filter(|select| matches!(select, TreeValue::SubObjects(_))).count();
            let num_pri_banks_selected = selection
                .iter()
                .filter(|select| matches!(select, TreeValue::Weapons(WeaponTreeValue::PriBank(_))))
                .count();
            let num_sec_banks_selected = selection
                .iter()
                .filter(|select| matches!(select, TreeValue::Weapons(WeaponTreeValue::SecBank(_))))
                .count();
            let num_docks_selected = selection.iter().filter(|select| matches!(select, TreeValue::DockingBays(_))).count();
            let num_thruster_banks_selected = selection.iter().filter(|select| matches!(select, TreeValue::Thrusters(_))).count();
            let num_glow_banks_selected = selection.iter().filter(|select| matches!(select, TreeValue::Glows(_))).count();
            let num_spc_points_selected = selection.iter().filter(|select| matches!(select, TreeValue::SpecialPoints(_))).count();
            let num_turrets_selected = selection.iter().filter(|select| matches!(select, TreeValue::Turrets(_))).count();
            let num_paths_selected = selection.iter().filter(|select| matches!(select, TreeValue::Paths(_))).count();
            let num_eyes_selected = selection.iter().filter(|select| matches!(select, TreeValue::EyePoints(_))).count();
            let num_insignias_selected = selection.iter().filter(|select| matches!(select, TreeValue::Insignia(_))).count();

            egui::SidePanel::left("left_panel")
                .default_width(250.0)
                .width_range(80.0..=250.0)
                .show_inside(ui, |ui| {
                    ui.label(
                        "Here you can import pof data from another model, either by adding it or matching it against \
                            existing data and replacing it.",
                    );

                    ui.checkbox(
                        &mut self.import_window.import_options.auto_select_subobj_children,
                        "Auto-select subobject children on subobject selection.",
                    );
                    ui.checkbox(&mut self.import_window.import_options.auto_select_paths, "Auto-select associated paths, if available.");
                    ui.checkbox(&mut self.import_window.import_options.auto_select_turrets, "Auto-select associated turret data, if available.");

                    ui.separator();

                    ui.horizontal(|ui| {
                        ui.selectable_value(
                            &mut self.import_window.import_type,
                            ImportType::Add,
                            RichText::new("Add").text_style(TextStyle::Heading),
                        );
                        ui.selectable_value(
                            &mut self.import_window.import_type,
                            ImportType::MatchAndReplace,
                            RichText::new("Match And Replace").text_style(TextStyle::Heading),
                        );
                    });

                    ui.add_space(5.0);

                    ui.horizontal(|ui| {
                        let path_string = self
                            .import_window
                            .model
                            .as_ref()
                            .map_or(String::new(), |model| model.path_to_file.display().to_string());
                        let path_string = match path_string.char_indices().nth_back(36) {
                            Some((n, _)) => format!("...{}", &path_string[(n + 3)..]),
                            None => path_string,
                        };
                        let mut pos = ui.next_widget_position();
                        pos.y -= ui.style().spacing.interact_size.y * 0.5;
                        let rect = egui::Rect::from_min_size(pos, [ui.available_width(), ui.available_height()].into());
                        ui.painter().rect_filled(rect, 2.0, ui.visuals().extreme_bg_color);
                        let response = ui.add(TextEdit::singleline(&mut &*path_string).hint_text("Model file...").desired_width(220.0));

                        let response = response.interact(egui::Sense::click());
                        let mut clicked_browse = response.clicked();

                        if self.import_window.import_model_loading_thread.is_some() {
                            ui.add(egui::widgets::Spinner::new());
                        } else {
                            clicked_browse |= ui.button("...").clicked();
                        }

                        if clicked_browse {
                            start_loading_import_model(&mut self.import_window.import_model_loading_thread);
                        }
                    });

                    ui.add_space(10.0);

                    egui::ScrollArea::vertical().auto_shrink([false, true]).show(ui, |ui| {
                        let selection = &self.import_window.import_selection;
                        let import_model = &self.import_window.model;

                        if selection.contains(&TreeValue::Header) {
                            ui.label("Header Geometry Data selected.");

                            if ImportType::Add == self.import_window.import_type {
                                ui.indent("header warnings", |ui| {
                                    ui.label(
                                        RichText::new(format!("Header geometry data cannot be added, it will be match and replaced instead."))
                                            .color(WARNING_YELLOW),
                                    );
                                });
                            }
                        }

                        if num_subobjects_selected > 0 {
                            ui.label(format!(
                                "{} SubObject{} selected.",
                                num_subobjects_selected,
                                if num_subobjects_selected == 1 { "" } else { "s" }
                            ));

                            let mut warnings = vec![];

                            if ImportType::MatchAndReplace == self.import_window.import_type {
                                for tree_val in selection.iter().filter(|select| matches!(select, TreeValue::SubObjects(_))) {
                                    if let TreeValue::SubObjects(SubObjectTreeValue::SubObject(id)) = tree_val {
                                        if !model
                                            .sub_objects
                                            .iter()
                                            .any(|subobj| subobj.name == import_model.as_ref().unwrap().sub_objects[*id].name)
                                        {
                                            warnings.push(
                                                RichText::new(format!(
                                                    "There is no match for subobject '{}' in the recieving model. It will be added instead.",
                                                    import_model.as_ref().unwrap().sub_objects[*id].name
                                                ))
                                                .color(WARNING_YELLOW),
                                            );
                                        }
                                    }
                                }
                            }

                            if !warnings.is_empty() {
                                ui.indent("subobject warnings", |ui| {
                                    for warning in warnings {
                                        ui.label(warning);
                                    }
                                });
                            }
                        }

                        if num_pri_banks_selected > 0 {
                            ui.label(format!(
                                "{} Primary Bank{} selected.",
                                num_pri_banks_selected,
                                if num_pri_banks_selected == 1 { "" } else { "s" }
                            ));
                        }

                        if num_sec_banks_selected > 0 {
                            ui.label(format!(
                                "{} Secondary Bank{} selected.",
                                num_sec_banks_selected,
                                if num_sec_banks_selected == 1 { "" } else { "s" }
                            ));
                        }

                        if num_docks_selected > 0 {
                            ui.label(format!("{} Docking bay{} selected.", num_docks_selected, if num_docks_selected == 1 { "" } else { "s" }));

                            let mut warnings = vec![];

                            for tree_val in selection.iter().filter(|select| matches!(select, TreeValue::DockingBays(_))) {
                                if let TreeValue::DockingBays(DockingTreeValue::Bay(idx)) = tree_val {
                                    let dock = &import_model.as_ref().unwrap().docking_bays[*idx];

                                    if let Some(id) = dock
                                        .get_parent_obj()
                                        .and_then(|submodel_str| import_model.as_ref().unwrap().get_obj_id_by_name(submodel_str))
                                    {
                                        if !self
                                            .import_window
                                            .import_selection
                                            .contains(&TreeValue::SubObjects(SubObjectTreeValue::SubObject(id)))
                                        {
                                            warnings.push(
                                                RichText::new(format!(
                                                    "Docking bay '{}' has a parent object which is not being imported. \
                                                         The parent object field will be removed.",
                                                    import_model.as_ref().unwrap().docking_bays[*idx]
                                                        .get_name()
                                                        .unwrap_or(&format!("Docking bay {}", idx)),
                                                ))
                                                .color(WARNING_YELLOW),
                                            );
                                        }
                                    }

                                    if self.import_window.import_type == ImportType::MatchAndReplace {
                                        if let Some(name) = dock.get_name() {
                                            if !model.docking_bays.iter().any(|other_dock| other_dock.get_name() == Some(name)) {
                                                warnings.push(
                                                    RichText::new(format!(
                                                        "There is no match for docking bay '{}' in the recieving model. \
                                                             It will be added instead.",
                                                        name
                                                    ))
                                                    .color(WARNING_YELLOW),
                                                );
                                            }
                                        } else {
                                            warnings.push(
                                                RichText::new(format!(
                                                    "Docking bay {} needs a name to be properly matched. \
                                                         It will be added instead.",
                                                    idx
                                                ))
                                                .color(WARNING_YELLOW),
                                            );
                                        }
                                    }
                                }
                            }

                            if !warnings.is_empty() {
                                ui.indent("dock warnings", |ui| {
                                    for warning in warnings {
                                        ui.label(warning);
                                    }
                                });
                            }
                        }

                        if num_thruster_banks_selected > 0 {
                            ui.label(format!(
                                "{} Thruster bank{} selected.",
                                num_thruster_banks_selected,
                                if num_thruster_banks_selected == 1 { "" } else { "s" }
                            ));

                            let mut warnings = vec![];

                            for tree_val in selection.iter().filter(|select| matches!(select, TreeValue::Thrusters(_))) {
                                if let TreeValue::Thrusters(ThrusterTreeValue::Bank(idx)) = tree_val {
                                    let import_model = &import_model.as_ref().unwrap();
                                    if let Some(subsys_name) = import_model.thruster_banks[*idx].get_engine_subsys() {
                                        let mut found_a_match = import_model.get_obj_id_by_name(subsys_name).map_or(false, |id| {
                                            !self
                                                .import_window
                                                .import_selection
                                                .contains(&TreeValue::SubObjects(SubObjectTreeValue::SubObject(id)))
                                        });

                                        found_a_match |= import_model
                                            .special_points
                                            .iter()
                                            .filter(|spc_point| spc_point.is_subsystem())
                                            .any(|spc_point| spc_point.name.strip_prefix('$').unwrap_or(&spc_point.name) == subsys_name);

                                        if !found_a_match {
                                            // engine subsys was not imported, lose it
                                            warnings.push(
                                                RichText::new(format!(
                                                    "There is no matching engine subsystem for thruster bank {} in the recieving model. \
                                                        It will be added instead.",
                                                    idx + 1
                                                ))
                                                .color(WARNING_YELLOW),
                                            );
                                        }
                                    } else {
                                        warnings.push(
                                            RichText::new(format!(
                                                "Thruster bank {} needs an associated engine subsystem to be properly matched. \
                                                    It will be added instead.",
                                                idx + 1
                                            ))
                                            .color(WARNING_YELLOW),
                                        );
                                    }
                                }
                            }

                            if !warnings.is_empty() {
                                ui.indent("thruster banks warnings", |ui| {
                                    for warning in warnings {
                                        ui.label(warning);
                                    }
                                });
                            }
                        }

                        if num_glow_banks_selected > 0 {
                            ui.label(format!(
                                "{} Glow bank{} selected.",
                                num_glow_banks_selected,
                                if num_glow_banks_selected == 1 { "" } else { "s" }
                            ));

                            let mut warnings = vec![];

                            if self.import_window.import_type == ImportType::MatchAndReplace {
                                warnings
                                    .push(RichText::new("Glow banks cannt be match and replaced. Any will be added instead.").color(WARNING_YELLOW));
                            }

                            for tree_val in selection.iter().filter(|select| matches!(select, TreeValue::Glows(_))) {
                                if let TreeValue::Glows(GlowTreeValue::Bank(idx)) = tree_val {
                                    let glow_bank = &import_model.as_ref().unwrap().glow_banks[*idx];
                                    if !self
                                        .import_window
                                        .import_selection
                                        .contains(&TreeValue::SubObjects(SubObjectTreeValue::SubObject(glow_bank.obj_parent)))
                                    {
                                        let was_already_on_detail0 =
                                            import_model.as_ref().unwrap().header.detail_levels.get(0) == Some(&glow_bank.obj_parent);
                                        if !was_already_on_detail0 {
                                            warnings.push(
                                                RichText::new(format!(
                                                    "Glow bank {}'s object parent is not being imported; \
                                                         it will be reset to the recieving model's detail0.",
                                                    idx + 1
                                                ))
                                                .color(WARNING_YELLOW),
                                            );
                                        }
                                    };
                                }
                            }

                            if !warnings.is_empty() {
                                ui.indent("glow banks warnings", |ui| {
                                    for warning in warnings {
                                        ui.label(warning);
                                    }
                                });
                            }
                        }

                        if num_spc_points_selected > 0 {
                            ui.label(format!(
                                "{} Special point{} selected.",
                                num_spc_points_selected,
                                if num_spc_points_selected == 1 { "" } else { "s" }
                            ));

                            let mut warnings = vec![];

                            for tree_val in selection.iter().filter(|select| matches!(select, TreeValue::SpecialPoints(_))) {
                                if let TreeValue::SpecialPoints(SpecialPointTreeValue::Point(idx)) = tree_val {
                                    let spc_point = &import_model.as_ref().unwrap().special_points[*idx];

                                    if self.import_window.import_type == ImportType::MatchAndReplace {
                                        if !model.special_points.iter().any(|other_point| other_point.name == spc_point.name) {
                                            warnings.push(
                                                RichText::new(format!(
                                                    "There is no match for special point '{}' in the recieving model. It will be added instead.",
                                                    spc_point.name
                                                ))
                                                .color(WARNING_YELLOW),
                                            );
                                        }
                                    }
                                }
                            }

                            if !warnings.is_empty() {
                                ui.indent("special point warnings", |ui| {
                                    for warning in warnings {
                                        ui.label(warning);
                                    }
                                });
                            }
                        }

                        if num_turrets_selected > 0 {
                            ui.label(format!("{} Turret{} selected.", num_turrets_selected, if num_turrets_selected == 1 { "" } else { "s" }));

                            let mut warnings = vec![];

                            for tree_val in selection.iter().filter(|select| matches!(select, TreeValue::Turrets(_))) {
                                if let TreeValue::Turrets(TurretTreeValue::Turret(idx)) = tree_val {
                                    let import_model = import_model.as_ref().unwrap();
                                    let turret = &import_model.turrets[*idx];

                                    match self.import_window.import_type {
                                        ImportType::Add => {
                                            if !selection.contains(&TreeValue::SubObjects(SubObjectTreeValue::SubObject(turret.base_obj)))
                                                || !selection.contains(&TreeValue::SubObjects(SubObjectTreeValue::SubObject(turret.gun_obj)))
                                            {
                                                // parent objects were not imported, see if we can find parents...
                                                let mut found_match = false;
                                                for sobj in &model.sub_objects {
                                                    let singlepart_valid = sobj.name == import_model.sub_objects[turret.gun_obj].name
                                                        && turret.gun_obj == turret.base_obj;
                                                    let multipart_valid = sobj.name == import_model.sub_objects[turret.gun_obj].name
                                                        && sobj.parent().map(|id| &model.sub_objects[id].name)
                                                            == Some(&import_model.sub_objects[turret.base_obj].name);

                                                    found_match = singlepart_valid || multipart_valid;
                                                    if found_match {
                                                        break;
                                                    }
                                                }

                                                if !found_match {
                                                    warnings.push(
                                                        RichText::new(format!(
                                                            "There is no match for turret '{}' in the recieving model. It will not be added.",
                                                            import_model.sub_objects[turret.base_obj].name
                                                        ))
                                                        .color(ERROR_RED),
                                                    );
                                                }
                                            }
                                        }
                                        ImportType::MatchAndReplace => {
                                            if model.turrets.iter().any(|other_turret| {
                                                model.sub_objects[other_turret.base_obj].name == import_model.sub_objects[turret.base_obj].name
                                            }) {
                                                if !selection.contains(&TreeValue::SubObjects(SubObjectTreeValue::SubObject(turret.base_obj))) {
                                                    warnings.push(
                                                        RichText::new(format!(
                                                            "There is no match for turret '{}' in the recieving model. It will not be added.",
                                                            import_model.sub_objects[turret.base_obj].name
                                                        ))
                                                        .color(ERROR_RED),
                                                    );
                                                } else {
                                                    warnings.push(
                                                        RichText::new(format!(
                                                            "There is no match for turret '{}' in the recieving model. It will be added instead.",
                                                            import_model.sub_objects[turret.base_obj].name
                                                        ))
                                                        .color(WARNING_YELLOW),
                                                    );
                                                }
                                            }
                                        }
                                    }
                                }
                            }

                            if !warnings.is_empty() {
                                ui.indent("turret warnings", |ui| {
                                    for warning in warnings {
                                        ui.label(warning);
                                    }
                                });
                            }
                        }

                        if num_paths_selected > 0 {
                            ui.label(format!("{} Path{} selected.", num_paths_selected, if num_paths_selected == 1 { "" } else { "s" }));

                            let mut warnings = vec![];

                            for tree_val in selection.iter().filter(|select| matches!(select, TreeValue::Paths(_))) {
                                if let TreeValue::Paths(PathTreeValue::Path(idx)) = tree_val {
                                    let path = &import_model.as_ref().unwrap().paths[*idx];

                                    if self.import_window.import_type == ImportType::MatchAndReplace {
                                        if !model.paths.iter().any(|other_path| other_path.name == path.name) {
                                            warnings.push(
                                                RichText::new(format!(
                                                    "There is no match for path '{}' in the recieving model. It will be added instead.",
                                                    path.name
                                                ))
                                                .color(WARNING_YELLOW),
                                            );
                                        }
                                    }
                                }
                            }

                            if !warnings.is_empty() {
                                ui.indent("turret warnings", |ui| {
                                    for warning in warnings {
                                        ui.label(warning);
                                    }
                                });
                            }
                        }

                        if num_eyes_selected > 0 {
                            ui.label(format!("{} Eye point{} selected.", num_eyes_selected, if num_eyes_selected == 1 { "" } else { "s" }));

                            let mut warnings = vec![];

                            for tree_val in selection.iter().filter(|select| matches!(select, TreeValue::EyePoints(_))) {
                                if let TreeValue::EyePoints(EyeTreeValue::EyePoint(idx)) = tree_val {
                                    let point = &import_model.as_ref().unwrap().eye_points[*idx];

                                    if self.import_window.import_type == ImportType::MatchAndReplace {
                                        let attached_obj = point.attached_subobj.map(|id| &import_model.as_ref().unwrap().sub_objects[id].name);

                                        if !model.eye_points.iter().any(|other_point| {
                                            other_point.attached_subobj.map(|id| &model.pof_model.sub_objects[id].name) == attached_obj
                                        }) {
                                            warnings.push(
                                                RichText::new(format!(
                                                    "There is no match for an eye point in the recieving model. It will be added instead."
                                                ))
                                                .color(WARNING_YELLOW),
                                            );
                                        }
                                    }
                                }
                            }

                            if !warnings.is_empty() {
                                ui.indent("eye warnings", |ui| {
                                    for warning in warnings {
                                        ui.label(warning);
                                    }
                                });
                            }
                        }

                        if num_insignias_selected > 0 {
                            ui.label(format!("{} Insignia selected.", num_insignias_selected));

                            if self.import_window.import_type == ImportType::MatchAndReplace
                                && selection.iter().any(|select| matches!(select, TreeValue::Insignia(_)))
                            {
                                ui.indent("insignia warnings", |ui| {
                                    ui.label(
                                        RichText::new(format!("Insignia cannot be match and replaced, any selected will be added instead."))
                                            .color(WARNING_YELLOW),
                                    );
                                });
                            }
                        }

                        if self.import_window.import_selection.contains(&TreeValue::Shield) {
                            ui.label(format!("Shield selected."));

                            if self.import_window.import_type == ImportType::Add && selection.iter().any(|select| matches!(select, TreeValue::Shield))
                            {
                                ui.indent("shield warnings", |ui| {
                                    ui.label(
                                        RichText::new(format!("The shield cannot be added, it will be match and replaced instead."))
                                            .color(WARNING_YELLOW),
                                    );
                                });
                            }
                        }
                    });

                    //ui.add_space(ui.available_height() - ui.spacing().interact_size.y * 2.0);
                    ui.add_space(10.0);
                    if ui.button(RichText::new("Confirm").text_style(TextStyle::Heading)).clicked() {
                        ret = true;
                    }
                    ui.add_space(5.0);
                });

            egui::ScrollArea::vertical().show(ui, |ui| {
                ui.allocate_space([300.0, 0.0].into());
                if let Some(import_model) = &self.import_window.model {
                    // make this into macro cause i do it a lot
                    // the only things that change are what gets iterated over, and what/how the enum that goes into the selection is made
                    macro_rules! get_selection_status {
                        ($x:expr, |$i:ident| $y:expr) => {{
                            let mut num_banks = 0;
                            let mut selected_banks = 0;
                            for $i in $x.iter().enumerate() {
                                num_banks += 1;
                                if self.import_window.import_selection.contains(&$y) {
                                    selected_banks += 1;
                                }
                            }

                            match (num_banks, selected_banks) {
                                (_, 0) => SelectionType::None,
                                (_, _) if selected_banks < num_banks => SelectionType::Partial,
                                (_, _) if selected_banks == num_banks => SelectionType::Total,
                                _ => unreachable!(),
                            }
                        }};
                    }

                    //Header
                    let selection_status = if self.import_window.import_selection.contains(&TreeValue::Header) {
                        SelectionType::Total
                    } else {
                        SelectionType::None
                    };
                    if selectable_label(ui, selection_status, "Header Geometry Data")
                        .on_hover_text("Bound box, radius, mass and moment of inertia")
                        .clicked()
                    {
                        toggle(&mut self.import_window.import_selection, TreeValue::Header);
                    }

                    // SubObjects
                    fn make_subobject_child_list(
                        import_model: &Model, selection: &mut BTreeSet<TreeValue>, object: &SubObject, ui: &mut Ui, bother_with_coloring: bool,
                        options: &ImportOptions,
                    ) {
                        let tree_val = TreeValue::SubObjects(SubObjectTreeValue::SubObject(object.obj_id));
                        let selection_status = {
                            if bother_with_coloring {
                                if selection.contains(&tree_val) {
                                    SelectionType::Total
                                } else {
                                    let mut num_subobjects = 0;
                                    let mut selected_subobjects = 0;
                                    import_model.do_for_recursive_subobj_children(object.obj_id, &mut |obj| {
                                        num_subobjects += 1;
                                        if selection.contains(&TreeValue::SubObjects(SubObjectTreeValue::SubObject(obj.obj_id))) {
                                            selected_subobjects += 1;
                                        }
                                    });

                                    match (num_subobjects, selected_subobjects) {
                                        (_, 0) => SelectionType::None,
                                        (_, _) if selected_subobjects < num_subobjects => SelectionType::Partial,
                                        (_, _) if selected_subobjects == num_subobjects => SelectionType::Total,
                                        _ => unreachable!(),
                                    }
                                }
                            } else {
                                SelectionType::None
                            }
                        };

                        if object.children().next().is_none() {
                            if selectable_label(ui, selection_status, &object.name).clicked() {
                                toggle(selection, tree_val);
                            }
                        } else {
                            let state = CollapsingState::load_with_default_open(ui.ctx(), Id::new(format!("import {}", tree_val)), false);
                            let header_is_open = state.is_open();
                            state
                                .show_header(ui, |ui| {
                                    if selectable_label(ui, selection_status, &object.name).clicked() {
                                        if selection.contains(&tree_val) {
                                            selection.remove(&tree_val);
                                        } else {
                                            selection.insert(tree_val);
                                        }

                                        if options.auto_select_subobj_children {
                                            // a bit confusing, but do_for_recursive_subobj_children will also run on the subobject you call it on
                                            // so save its selection status now, rather than accidentally toggle it back to its old status
                                            let enable_disable = selection.contains(&tree_val);
                                            import_model.do_for_recursive_subobj_children(object.obj_id, &mut |obj| {
                                                set(selection, enable_disable, TreeValue::SubObjects(SubObjectTreeValue::SubObject(obj.obj_id)));
                                            });
                                        }
                                        if options.auto_select_turrets {
                                            for (i, turret) in import_model.turrets.iter().enumerate() {
                                                if turret.base_obj == object.obj_id {
                                                    set(selection, selection.contains(&tree_val), TreeValue::Turrets(TurretTreeValue::Turret(i)));
                                                }
                                            }
                                        }
                                        if options.auto_select_paths {
                                            for (i, path) in import_model.paths.iter().enumerate() {
                                                if path.parent.to_lowercase() == object.name.to_lowercase() {
                                                    set(selection, selection.contains(&tree_val), TreeValue::Paths(PathTreeValue::Path(i)));
                                                }
                                            }
                                        }
                                    }
                                })
                                .body(|ui| {
                                    for &i in object.children() {
                                        make_subobject_child_list(import_model, selection, &import_model.sub_objects[i], ui, header_is_open, options)
                                    }
                                });
                        }
                    }

                    if !import_model.sub_objects.is_empty() {
                        let state = CollapsingState::load_with_default_open(ui.ctx(), Id::new("import SubObjects"), false);
                        let header_is_open = state.is_open();
                        let selection_status =
                            get_selection_status!(import_model.sub_objects, |i| TreeValue::SubObjects(SubObjectTreeValue::SubObject(i.1.obj_id)));
                        state
                            .show_header(ui, |ui| {
                                if selectable_label(ui, selection_status, format!("SubObjects ({})", import_model.sub_objects.len())).clicked() {
                                    if num_subobjects_selected == import_model.sub_objects.len() {
                                        for object in &import_model.sub_objects {
                                            self.import_window
                                                .import_selection
                                                .remove(&TreeValue::SubObjects(SubObjectTreeValue::SubObject(object.obj_id)));
                                        }
                                    } else {
                                        for object in &import_model.sub_objects {
                                            self.import_window
                                                .import_selection
                                                .insert(TreeValue::SubObjects(SubObjectTreeValue::SubObject(object.obj_id)));
                                        }
                                    }
                                }
                            })
                            .body(|ui| {
                                for object in &import_model.sub_objects {
                                    if object.parent().is_none() {
                                        make_subobject_child_list(
                                            import_model,
                                            &mut self.import_window.import_selection,
                                            object,
                                            ui,
                                            header_is_open,
                                            &self.import_window.import_options,
                                        );
                                    }
                                }
                            });
                    }

                    macro_rules! header_stuff {
                        (($status:expr, $list:ident, $selected:expr), $text:expr, |$i:ident| $enum:expr) => {{
                            |ui| {
                                if selectable_label(ui, $status, $text).clicked() {
                                    if $selected == import_model.$list.len() {
                                        for $i in import_model.$list.iter().enumerate() {
                                            self.import_window.import_selection.remove(&$enum);
                                        }
                                    } else {
                                        for $i in import_model.$list.iter().enumerate() {
                                            self.import_window.import_selection.insert($enum);
                                        }
                                    }
                                }
                            }
                        }};
                    }

                    macro_rules! body_stuff {
                        ($list:ident, |$bank:ident| $text:expr, |$i:ident| $enum:expr) => {{
                            |ui| {
                                for ($i, $bank) in import_model.$list.iter().enumerate() {
                                    let tree_val = $enum;
                                    let selection_status = if self.import_window.import_selection.contains(&tree_val) {
                                        SelectionType::Total
                                    } else {
                                        SelectionType::None
                                    };
                                    if selectable_label(ui, selection_status, $text).clicked() {
                                        toggle(&mut self.import_window.import_selection, tree_val);
                                    }
                                }
                            }
                        }};
                    }

                    if !import_model.primary_weps.is_empty() {
                        let state = CollapsingState::load_with_default_open(ui.ctx(), Id::new("import Primary Weapons"), false);
                        let selection_status =
                            get_selection_status!(import_model.primary_weps, |i| TreeValue::Weapons(WeaponTreeValue::PriBank(i.0)));
                        state
                            .show_header(
                                ui,
                                header_stuff!(
                                    (selection_status, primary_weps, num_pri_banks_selected),
                                    format!("Primary Banks ({})", import_model.primary_weps.len()),
                                    |i| TreeValue::Weapons(WeaponTreeValue::PriBank(i.0))
                                ),
                            )
                            .body(body_stuff!(
                                primary_weps,
                                |bank| format!("Primary Bank {} ({} point{})", i + 1, bank.len(), if bank.len() == 1 { "" } else { "s" }),
                                |i| TreeValue::Weapons(WeaponTreeValue::PriBank(i))
                            ));
                    }

                    if !import_model.secondary_weps.is_empty() {
                        let state = CollapsingState::load_with_default_open(ui.ctx(), Id::new("import Secondary Weapons"), false);
                        let selection_status =
                            get_selection_status!(import_model.secondary_weps, |i| TreeValue::Weapons(WeaponTreeValue::SecBank(i.0)));
                        state
                            .show_header(
                                ui,
                                header_stuff!(
                                    (selection_status, secondary_weps, num_sec_banks_selected),
                                    format!("Secondary Banks ({})", import_model.secondary_weps.len()),
                                    |i| TreeValue::Weapons(WeaponTreeValue::SecBank(i.0))
                                ),
                            )
                            .body(body_stuff!(
                                secondary_weps,
                                |bank| format!("Secondary Bank {} ({} point{})", i + 1, bank.len(), if bank.len() == 1 { "" } else { "s" }),
                                |i| TreeValue::Weapons(WeaponTreeValue::SecBank(i))
                            ));
                    }

                    if !import_model.docking_bays.is_empty() {
                        let state = CollapsingState::load_with_default_open(ui.ctx(), Id::new("import Docking Bay"), false);
                        let selection_status =
                            get_selection_status!(import_model.docking_bays, |i| TreeValue::DockingBays(DockingTreeValue::Bay(i.0)));
                        state
                            .show_header(
                                ui,
                                header_stuff!(
                                    (selection_status, docking_bays, num_docks_selected),
                                    format!("Docking Bays ({})", import_model.docking_bays.len()),
                                    |i| TreeValue::DockingBays(DockingTreeValue::Bay(i.0))
                                ),
                            )
                            .body(body_stuff!(docking_bays, |bank| format!("{}", bank.get_name().unwrap_or("Unnamed Dock")), |i| {
                                TreeValue::DockingBays(DockingTreeValue::Bay(i))
                            }));
                    }

                    if !import_model.thruster_banks.is_empty() {
                        let state = CollapsingState::load_with_default_open(ui.ctx(), Id::new("import Thruster Bank"), false);
                        let selection_status =
                            get_selection_status!(import_model.thruster_banks, |i| TreeValue::Thrusters(ThrusterTreeValue::Bank(i.0)));
                        state
                            .show_header(
                                ui,
                                header_stuff!(
                                    (selection_status, thruster_banks, num_thruster_banks_selected),
                                    format!("Thruster Banks ({})", import_model.thruster_banks.len()),
                                    |i| TreeValue::Thrusters(ThrusterTreeValue::Bank(i.0))
                                ),
                            )
                            .body(body_stuff!(
                                thruster_banks,
                                |bank| format!(
                                    "Thruster Bank {} ({} point{})",
                                    i + 1,
                                    bank.glows.len(),
                                    if bank.glows.len() == 1 { "" } else { "s" }
                                ),
                                |i| TreeValue::Thrusters(ThrusterTreeValue::Bank(i))
                            ));
                    }

                    if !import_model.glow_banks.is_empty() {
                        let state = CollapsingState::load_with_default_open(ui.ctx(), Id::new("import Glow Bank"), false);
                        let selection_status = get_selection_status!(import_model.glow_banks, |i| TreeValue::Glows(GlowTreeValue::Bank(i.0)));
                        state
                            .show_header(
                                ui,
                                header_stuff!(
                                    (selection_status, glow_banks, num_glow_banks_selected),
                                    format!("Glow Banks ({})", import_model.glow_banks.len()),
                                    |i| TreeValue::Glows(GlowTreeValue::Bank(i.0))
                                ),
                            )
                            .body(body_stuff!(
                                glow_banks,
                                |bank| format!(
                                    "Glow Bank {} ({} point{})",
                                    i + 1,
                                    bank.glow_points.len(),
                                    if bank.glow_points.len() == 1 { "" } else { "s" }
                                ),
                                |i| TreeValue::Glows(GlowTreeValue::Bank(i))
                            ));
                    }

                    if !import_model.special_points.is_empty() {
                        let state = CollapsingState::load_with_default_open(ui.ctx(), Id::new("import Special Points"), false);
                        let selection_status =
                            get_selection_status!(import_model.special_points, |i| TreeValue::SpecialPoints(SpecialPointTreeValue::Point(i.0)));
                        state
                            .show_header(
                                ui,
                                header_stuff!(
                                    (selection_status, special_points, num_spc_points_selected),
                                    format!("Special Points ({})", import_model.special_points.len()),
                                    |i| TreeValue::SpecialPoints(SpecialPointTreeValue::Point(i.0))
                                ),
                            )
                            .body(
                                //body_stuff!(special_points, |point| format!("{}", point.name), |i| TreeValue::SpecialPoints(SpecialPointTreeValue::Point(i)))
                                |ui| {
                                    for (i, point) in import_model.special_points.iter().enumerate() {
                                        let tree_val = TreeValue::SpecialPoints(SpecialPointTreeValue::Point(i));
                                        let selection_status = if self.import_window.import_selection.contains(&tree_val) {
                                            SelectionType::Total
                                        } else {
                                            SelectionType::None
                                        };
                                        if selectable_label(ui, selection_status, format!("{}", point.name)).clicked() {
                                            let selection = &mut self.import_window.import_selection;
                                            toggle(selection, tree_val);

                                            if self.import_window.import_options.auto_select_paths {
                                                for (i, path) in import_model.paths.iter().enumerate() {
                                                    if path.parent == point.name {
                                                        set(selection, selection.contains(&tree_val), TreeValue::Paths(PathTreeValue::Path(i)));
                                                    }
                                                }
                                            }
                                        }
                                    }
                                },
                            );
                    }

                    if !import_model.turrets.is_empty() {
                        let state = CollapsingState::load_with_default_open(ui.ctx(), Id::new("import Turrets"), false);
                        let selection_status = get_selection_status!(import_model.turrets, |i| TreeValue::Turrets(TurretTreeValue::Turret(i.0)));
                        state
                            .show_header(
                                ui,
                                header_stuff!(
                                    (selection_status, turrets, num_turrets_selected),
                                    format!("Turrets ({})", import_model.turrets.len()),
                                    |i| TreeValue::Turrets(TurretTreeValue::Turret(i.0))
                                ),
                            )
                            .body(body_stuff!(turrets, |turret| format!("{}", import_model.sub_objects[turret.base_obj].name), |i| {
                                TreeValue::Turrets(TurretTreeValue::Turret(i))
                            }));
                    }

                    if !import_model.paths.is_empty() {
                        let state = CollapsingState::load_with_default_open(ui.ctx(), Id::new("import Paths"), false);
                        let selection_status = get_selection_status!(import_model.paths, |i| TreeValue::Paths(PathTreeValue::Path(i.0)));
                        state
                            .show_header(
                                ui,
                                header_stuff!((selection_status, paths, num_paths_selected), format!("Paths ({})", import_model.paths.len()), |i| {
                                    TreeValue::Paths(PathTreeValue::Path(i.0))
                                }),
                            )
                            .body(body_stuff!(paths, |path| format!("{}", path.name), |i| TreeValue::Paths(PathTreeValue::Path(i))));
                    }

                    if import_model.shield_data.is_some()
                        && selectable_label(
                            ui,
                            if self.import_window.import_selection.contains(&TreeValue::Shield) {
                                SelectionType::Total
                            } else {
                                SelectionType::None
                            },
                            "Shield",
                        )
                        .clicked()
                    {
                        toggle(&mut self.import_window.import_selection, TreeValue::Shield);
                    }

                    if !import_model.eye_points.is_empty() {
                        let state = CollapsingState::load_with_default_open(ui.ctx(), Id::new("import Eye points"), false);
                        let selection_status = get_selection_status!(import_model.eye_points, |i| TreeValue::EyePoints(EyeTreeValue::EyePoint(i.0)));
                        state
                            .show_header(
                                ui,
                                header_stuff!(
                                    (selection_status, eye_points, num_eyes_selected),
                                    format!("Eye points ({})", import_model.eye_points.len()),
                                    |i| TreeValue::EyePoints(EyeTreeValue::EyePoint(i.0))
                                ),
                            )
                            .body(body_stuff!(eye_points, |_point| format!("Eye Point {}", i + 1), |i| TreeValue::EyePoints(
                                EyeTreeValue::EyePoint(i)
                            )));
                    }

                    if !import_model.insignias.is_empty() {
                        let state = CollapsingState::load_with_default_open(ui.ctx(), Id::new("import Insignias"), false);
                        let selection_status =
                            get_selection_status!(import_model.insignias, |i| TreeValue::Insignia(InsigniaTreeValue::Insignia(i.0)));
                        state
                            .show_header(
                                ui,
                                header_stuff!(
                                    (selection_status, insignias, num_insignias_selected),
                                    format!("Insignia ({})", import_model.insignias.len()),
                                    |i| TreeValue::Insignia(InsigniaTreeValue::Insignia(i.0))
                                ),
                            )
                            .body(body_stuff!(insignias, |_insig| format!("Insignia {}", i + 1), |i| TreeValue::Insignia(
                                InsigniaTreeValue::Insignia(i)
                            )));
                    }
                } else {
                    ui.weak("No model selected to import");
                }
            });
        });

        ret
    }
}

fn toggle<T: Ord>(btree: &mut BTreeSet<T>, val: T) {
    if btree.contains(&val) {
        btree.remove(&val);
    } else {
        btree.insert(val);
    }
}

fn set<T: Ord>(btree: &mut BTreeSet<T>, enable: bool, val: T) {
    if enable {
        btree.insert(val);
    } else {
        btree.remove(&val);
    }
}

fn selectable_label(ui: &mut Ui, selection: SelectionType, label: impl Into<WidgetText>) -> Response {
    let mut button = Button::new(label);
    match selection {
        SelectionType::Total => {
            button = button.fill(Color32::from_rgb(0, 92, 128));
            ui.visuals_mut().override_text_color = Some(Color32::from_rgb(173, 202, 233));
        }
        SelectionType::Partial => {
            button = button.fill(ui.style_mut().visuals.widgets.noninteractive.bg_fill);
            button = button.stroke((1.0, Color32::from_rgb(144, 209, 255)));
        }
        SelectionType::None => button = button.fill(ui.style_mut().visuals.widgets.noninteractive.bg_fill),
    }
    let response = ui.add(button);
    ui.visuals_mut().override_text_color = None;

    response
}

impl PofToolsGui {
    pub fn merge_import_model(&mut self) {
        // this does a lot of much quicker std::mem:takes instead of clones
        // this will mangle the import_model so we must take it by value
        let selection = std::mem::take(&mut self.import_window.import_selection);
        let mut import_model = std::mem::take(&mut self.import_window.model).unwrap();

        // make the object id map to translate old object ids to new object ids
        let mut obj_id_map = HashMap::new();
        let mut num_subobjects = self.model.sub_objects.len();
        for tree_val in &selection {
            if let TreeValue::SubObjects(SubObjectTreeValue::SubObject(id)) = *tree_val {
                let new_id = match self.import_window.import_type {
                    ImportType::Add => {
                        num_subobjects += 1;
                        ObjectId((num_subobjects - 1) as u32)
                    }
                    ImportType::MatchAndReplace => {
                        let mut replaced_id = None;
                        for subobj in &self.model.sub_objects {
                            if subobj.name == import_model.sub_objects[id].name {
                                replaced_id = Some(subobj.obj_id);
                                break;
                            }
                        }

                        if let Some(replaced_id) = replaced_id {
                            replaced_id
                        } else {
                            //uh oh, just add instead?
                            num_subobjects += 1;
                            ObjectId((num_subobjects - 1) as u32)
                        }
                    }
                };
                obj_id_map.insert(id, new_id);
            }
        }

        // we're mangling the import_model as we go, so order is VERY IMPORTANT
        // do the things which require the subobjects still be intact
        for tree_val in &selection {
            match *tree_val {
                TreeValue::Header => {
                    let header = std::mem::take(&mut import_model.header);

                    self.model.header.bbox = header.bbox;
                    self.model.header.max_radius = header.max_radius;
                    self.model.header.mass = header.mass;
                    self.model.header.moment_of_inertia = header.moment_of_inertia;
                }
                TreeValue::DockingBays(DockingTreeValue::Bay(idx)) => {
                    // requires getting subobjects by name -> still have to be intact
                    let mut dock = std::mem::take(&mut import_model.docking_bays[idx]);
                    if let Some(parent_name) = dock.get_parent_obj() {
                        if import_model
                            .get_obj_id_by_name(parent_name)
                            .map_or(true, |id| !obj_id_map.contains_key(&id))
                        {
                            // parent submodel was not imported, lose it
                            properties_delete_field(&mut dock.properties, "$parent_submodel");
                        }
                    }

                    match self.import_window.import_type {
                        ImportType::Add => {
                            self.model.docking_bays.push(dock);
                        }
                        ImportType::MatchAndReplace => {
                            // find and replace
                            if let Some(name) = dock.get_name() {
                                if let Some(replaced_dock) = self
                                    .model
                                    .docking_bays
                                    .iter_mut()
                                    .find(|replaced_dock| replaced_dock.get_name() == Some(name))
                                {
                                    *replaced_dock = dock;
                                } else {
                                    // fall back, just add it
                                    self.model.docking_bays.push(dock);
                                }
                            }
                        }
                    }
                }
                TreeValue::DockingBays(_) => unreachable!(),
                TreeValue::Thrusters(ThrusterTreeValue::Bank(idx)) => {
                    // requires getting subobjects by name -> still have to be intact
                    let mut t_bank = std::mem::take(&mut import_model.thruster_banks[idx]);
                    if let Some(subsys_name) = t_bank.get_engine_subsys() {
                        let mut found_a_match = import_model
                            .get_obj_id_by_name(subsys_name)
                            .map_or(false, |id| !obj_id_map.contains_key(&id));

                        found_a_match |= import_model
                            .special_points
                            .iter()
                            .filter(|spc_point| spc_point.is_subsystem())
                            .any(|spc_point| spc_point.name.strip_prefix('$').unwrap_or(&spc_point.name) == subsys_name);

                        if !found_a_match {
                            // engine subsys was not imported, lose it
                            properties_delete_field(&mut t_bank.properties, "$engine_subsystem");
                        }
                    }

                    match self.import_window.import_type {
                        ImportType::Add => {
                            self.model.thruster_banks.push(t_bank);
                        }
                        ImportType::MatchAndReplace => {
                            // find and replace
                            if let Some(subsys_name) = t_bank.get_engine_subsys() {
                                if let Some(replaced_bank) = self
                                    .model
                                    .thruster_banks
                                    .iter_mut()
                                    .find(|replaced_bank| replaced_bank.get_engine_subsys() == Some(subsys_name))
                                {
                                    *replaced_bank = t_bank;
                                } else {
                                    // fall back, just add it
                                    self.model.thruster_banks.push(t_bank);
                                }
                            }
                        }
                    }
                }
                TreeValue::Thrusters(_) => unreachable!(),
                TreeValue::Glows(GlowTreeValue::Bank(idx)) => {
                    let mut g_bank = std::mem::take(&mut import_model.glow_banks[idx]);

                    if !obj_id_map.contains_key(&g_bank.obj_parent) {
                        // reset it to detail0
                        // ... if theres no detail0, just the first subobject
                        // ... if theres no objects, skip it i guess
                        if let Some(id) = self
                            .model
                            .header
                            .detail_levels
                            .get(0)
                            .or_else(|| self.model.sub_objects.get(0).map(|sobj| &sobj.obj_id))
                        {
                            g_bank.obj_parent = *id;
                        } else {
                            continue;
                        }
                    }

                    self.model.glow_banks.push(g_bank);
                }
                TreeValue::Glows(_) => unreachable!(),
                TreeValue::Turrets(TurretTreeValue::Turret(idx)) => {
                    let mut turret = std::mem::take(&mut import_model.turrets[idx]);

                    match self.import_window.import_type {
                        ImportType::Add => {
                            if selection.contains(&TreeValue::SubObjects(SubObjectTreeValue::SubObject(turret.base_obj)))
                                && selection.contains(&TreeValue::SubObjects(SubObjectTreeValue::SubObject(turret.gun_obj)))
                            {
                                // parent objects were imported too, cool
                                turret.base_obj = obj_id_map[&turret.base_obj];
                                turret.gun_obj = obj_id_map[&turret.gun_obj];
                            } else {
                                // parent objects were not imported, see if we can find parents...
                                let mut found_match = false;
                                for sobj in &self.model.sub_objects {
                                    let singlepart_valid =
                                        sobj.name == import_model.sub_objects[turret.gun_obj].name && turret.gun_obj == turret.base_obj;
                                    let multipart_valid = sobj.name == import_model.sub_objects[turret.gun_obj].name
                                        && sobj.parent().map(|id| &self.model.sub_objects[id].name)
                                            == Some(&import_model.sub_objects[turret.base_obj].name);

                                    if singlepart_valid {
                                        found_match = true;
                                        turret.gun_obj = sobj.obj_id;
                                        turret.base_obj = sobj.obj_id;
                                    } else if multipart_valid {
                                        found_match = true;
                                        turret.gun_obj = sobj.obj_id;
                                        turret.base_obj = sobj.parent().unwrap();
                                    }
                                }

                                if !found_match {
                                    continue;
                                }
                            }
                            self.model.turrets.push(turret);
                        }
                        ImportType::MatchAndReplace => {
                            // find and replace
                            if let Some(replaced_turret) = self.model.pof_model.turrets.iter_mut().find(|replaced_turret| {
                                self.model.pof_model.sub_objects[replaced_turret.base_obj].name == import_model.sub_objects[turret.base_obj].name
                            }) {
                                turret.base_obj = replaced_turret.base_obj;
                                turret.gun_obj = if obj_id_map.contains_key(&turret.gun_obj) {
                                    obj_id_map[&turret.gun_obj] // prefer an imported gun obj, instead of the replace turret's
                                } else {
                                    replaced_turret.gun_obj
                                };
                                *replaced_turret = turret;
                            } else if selection.contains(&TreeValue::SubObjects(SubObjectTreeValue::SubObject(turret.base_obj)))
                                && selection.contains(&TreeValue::SubObjects(SubObjectTreeValue::SubObject(turret.gun_obj)))
                            {
                                // fall back, try to add it
                                // parent objects were imported too, cool
                                turret.base_obj = obj_id_map[&turret.base_obj];
                                turret.gun_obj = obj_id_map[&turret.gun_obj];
                                self.model.turrets.push(turret);
                            }
                            // else just lose it
                        }
                    }
                }
                TreeValue::Turrets(_) => unreachable!(),
                TreeValue::EyePoints(EyeTreeValue::EyePoint(idx)) => {
                    let mut point = std::mem::take(&mut import_model.eye_points[idx]);

                    match self.import_window.import_type {
                        ImportType::Add => {
                            self.model.eye_points.push(point);
                        }
                        ImportType::MatchAndReplace => {
                            let attached_obj = point.attached_subobj.map(|id| &import_model.sub_objects[id].name);
                            // find and replace
                            if let Some(replaced_point) = self.model.pof_model.eye_points.iter_mut().find(|replaced_point| {
                                replaced_point.attached_subobj.map(|id| &self.model.pof_model.sub_objects[id].name) == attached_obj
                            }) {
                                point.attached_subobj = replaced_point.attached_subobj;
                                *replaced_point = point;
                            } else {
                                // fall back, just add it
                                self.model.eye_points.push(point);
                            }
                        }
                    }
                }
                TreeValue::Shield => {
                    let shield = std::mem::take(&mut import_model.shield_data);

                    if let Some(data) = shield {
                        self.model.shield_data = Some(data);
                    }
                }
                TreeValue::Insignia(InsigniaTreeValue::Insignia(idx)) => {
                    let insignia = std::mem::take(&mut import_model.insignias[idx]);

                    // uh what if its attached to a detail level the model doesnt have>????

                    self.model.insignias.push(insignia);
                }
                TreeValue::Insignia(_) => unreachable!(),
                TreeValue::VisualCenter => unreachable!(), // should this be importable?
                TreeValue::Comments => unreachable!(),     // should this be importable?
                TreeValue::SubObjects(_) => (),
                TreeValue::Weapons(_) => (),
                TreeValue::Textures(_) => unreachable!(),
                _ => (),
            }
        }

        let old_subobj_len = self.model.sub_objects.len();
        for tree_val in &selection {
            match *tree_val {
                TreeValue::SubObjects(SubObjectTreeValue::SubObject(id)) => {
                    // translate old texure ids to new one
                    let mut tex_id_map = HashMap::new();
                    for (_, poly) in import_model.pof_model.sub_objects[id].bsp_data.collision_tree.leaves_mut() {
                        if let Entry::Vacant(e) = tex_id_map.entry(poly.texture) {
                            // see if this texture already exists
                            if let Some(i) = (self.model.textures.iter())
                                .position(|tex_name| import_model.pof_model.textures[poly.texture.0 as usize] == *tex_name)
                            {
                                e.insert(TextureId(i as u32));
                            } else {
                                // still here, gotta add a slot i guess
                                e.insert(TextureId(self.model.textures.len() as u32));
                                self.model.textures.push(import_model.pof_model.textures[poly.texture.0 as usize].clone());
                            }
                        }

                        poly.texture = tex_id_map[&poly.texture];
                    }

                    let import_subobj = &mut import_model.sub_objects[id];
                    import_subobj.obj_id = obj_id_map[&id];

                    // make da swap (or addition)
                    if import_subobj.obj_id.0 as usize >= old_subobj_len {
                        if let Some(parent_id) = import_subobj.parent {
                            import_subobj.parent = obj_id_map.get(&parent_id).copied();
                        }

                        self.model.sub_objects.push(std::mem::take(import_subobj));
                        self.model.header.num_subobjects += 1;
                    } else {
                        let target_subobj = &mut self.model.sub_objects[import_subobj.obj_id];
                        // if you're replacing a subobj, you inherit the parent of what you've replaced
                        import_subobj.parent = target_subobj.parent;

                        // also inherit position
                        import_subobj.offset = target_subobj.offset;

                        *target_subobj = std::mem::take(import_subobj);
                    }
                }
                TreeValue::Weapons(WeaponTreeValue::PriBank(idx)) => {
                    self.model.primary_weps.push(std::mem::take(&mut import_model.primary_weps[idx]))
                }
                TreeValue::Weapons(WeaponTreeValue::SecBank(idx)) => {
                    self.model.secondary_weps.push(std::mem::take(&mut import_model.secondary_weps[idx]))
                }
                TreeValue::SpecialPoints(SpecialPointTreeValue::Point(idx)) => {
                    let spc_point = std::mem::take(&mut import_model.special_points[idx]);

                    match self.import_window.import_type {
                        ImportType::Add => {
                            self.model.special_points.push(spc_point);
                        }
                        ImportType::MatchAndReplace => {
                            if let Some(replaced_point) = self
                                .model
                                .special_points
                                .iter_mut()
                                .find(|replaced_point| replaced_point.name == spc_point.name)
                            {
                                *replaced_point = spc_point;
                            } else {
                                // fall back, just add it
                                self.model.special_points.push(spc_point);
                            }
                        }
                    }
                }
                TreeValue::SpecialPoints(_) => unreachable!(),
                TreeValue::Paths(PathTreeValue::Path(idx)) => {
                    let path = std::mem::take(&mut import_model.paths[idx]);

                    match self.import_window.import_type {
                        ImportType::Add => {
                            self.model.paths.push(path);
                        }
                        ImportType::MatchAndReplace => {
                            if let Some(replaced_path) = self.model.paths.iter_mut().find(|replaced_path| replaced_path.name == path.name) {
                                *replaced_path = path;
                            } else {
                                // fall back, just add it
                                self.model.paths.push(path);
                            }
                        }
                    }
                }
                TreeValue::Paths(_) => unreachable!(),
                _ => (),
            }
        }

        self.model.recalc_semantic_name_links();
        self.model.recalc_all_children_ids();
    }
}
