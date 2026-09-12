//! Tests for the stored indices every loader is expected to hand over already checked.

use pof::*;

/// A model with `paths` paths and `submodels` submodels, so a stored index has something to be in
/// range of. Nothing else about them matters here.
fn model_with(paths: u32, submodels: u32) -> Model {
    let mut model = Model::default();
    model.paths = (0..paths).map(|i| Path { name: format!("$path{:02}", i + 1), ..Default::default() }).collect();
    model.submodels = SubmodelVec(
        (0..submodels)
            .map(|i| Submodel { id: SubmodelId(i), name: format!("smodel{}", i), ..Default::default() })
            .collect(),
    );
    model
}

// ---------------------------------------------------------------- docking bay path links

#[test]
fn a_dock_link_naming_no_path_is_cleared() {
    let mut model = model_with(2, 0);
    model.docking_bays.push(Dock { path: Some(PathId(0)), ..Default::default() });
    model.docking_bays.push(Dock { path: Some(PathId(1)), ..Default::default() });
    model.docking_bays.push(Dock { path: Some(PathId(2)), ..Default::default() });
    model.docking_bays.push(Dock { path: Some(PathId(7)), ..Default::default() });
    model.docking_bays.push(Dock { path: None, ..Default::default() });

    model.sanitize_dock_paths();

    let links: Vec<_> = model.docking_bays.iter().map(|bay| bay.path).collect();
    assert_eq!(links, vec![Some(PathId(0)), Some(PathId(1)), None, None, None]);
}

#[test]
fn a_link_to_the_last_path_survives() {
    // the boundary the check turns on - one past the last valid index is the first invalid one
    let mut model = model_with(1, 0);
    model.docking_bays.push(Dock { path: Some(PathId(0)), ..Default::default() });
    model.sanitize_dock_paths();
    assert_eq!(model.docking_bays[0].path, Some(PathId(0)));
}

#[test]
fn every_link_goes_when_there_are_no_paths_at_all() {
    // the state an import produces when it read a bay's path number but no #paths node
    let mut model = model_with(0, 0);
    model.docking_bays.push(Dock { path: Some(PathId(0)), ..Default::default() });
    model.sanitize_dock_paths();
    assert_eq!(model.docking_bays[0].path, None);
}

// ---------------------------------------------------------------- eye point submodels

#[test]
fn an_eye_point_attached_to_no_submodel_is_cleared() {
    let mut model = model_with(0, 2);
    model.eye_points.push(EyePoint { attached_submodel: Some(SubmodelId(1)), ..Default::default() });
    model.eye_points.push(EyePoint { attached_submodel: Some(SubmodelId(2)), ..Default::default() });
    model.eye_points.push(EyePoint { attached_submodel: None, ..Default::default() });

    model.sanitize_eye_points();

    let attached: Vec<_> = model.eye_points.iter().map(|eye| eye.attached_submodel).collect();
    assert_eq!(attached, vec![Some(SubmodelId(1)), None, None]);
}

// ---------------------------------------------------------------- glow point bank parents

#[test]
fn a_glow_bank_parented_to_no_submodel_is_repointed() {
    // a bank is always attached to something, so there's no None to fall back to - it goes to the
    // first submodel instead, which is somewhere the user can see it and move it from
    let mut model = model_with(0, 2);
    model.glow_banks.push(GlowPointBank { model_parent: SubmodelId(1), ..Default::default() });
    model.glow_banks.push(GlowPointBank { model_parent: SubmodelId(9), ..Default::default() });

    model.sanitize_glow_bank_parents();

    let parents: Vec<_> = model.glow_banks.iter().map(|bank| bank.model_parent).collect();
    assert_eq!(parents, vec![SubmodelId(1), SubmodelId(0)]);
}

#[test]
fn a_glow_bank_is_dropped_when_there_is_nowhere_to_put_it() {
    // a model with no submodels at all has nothing for a bank to attach to; keeping one with a
    // dangling parent panics the moment it's drawn, so it's dropped rather than left behind
    let mut model = model_with(0, 0);
    model.glow_banks.push(GlowPointBank { model_parent: SubmodelId(3), ..Default::default() });
    model.sanitize_glow_bank_parents();
    assert!(model.glow_banks.is_empty(), "must not leave a bank pointing past the end of an empty submodel list");
}

// ---------------------------------------------------------------- the whole pass

#[test]
fn the_loader_pass_settles_every_kind_of_index_at_once() {
    // what the three loaders actually call. Each kind has its own test above; this is the one which
    // fails if a new kind is given a sanitizer but never wired into the pass
    let mut model = model_with(1, 1);
    model.docking_bays.push(Dock { path: Some(PathId(4)), ..Default::default() });
    model.eye_points.push(EyePoint { attached_submodel: Some(SubmodelId(4)), ..Default::default() });
    model.glow_banks.push(GlowPointBank { model_parent: SubmodelId(4), ..Default::default() });

    model.sanitize_index_references();

    assert_eq!(model.docking_bays[0].path, None);
    assert_eq!(model.eye_points[0].attached_submodel, None);
    assert_eq!(model.glow_banks[0].model_parent, SubmodelId(0));
}

#[test]
fn the_pass_changes_nothing_the_second_time_over() {
    let mut model = model_with(1, 1);
    model.docking_bays.push(Dock { path: Some(PathId(0)), ..Default::default() });
    model.docking_bays.push(Dock { path: Some(PathId(4)), ..Default::default() });
    model.eye_points.push(EyePoint { attached_submodel: Some(SubmodelId(4)), ..Default::default() });
    model.glow_banks.push(GlowPointBank { model_parent: SubmodelId(4), ..Default::default() });

    model.sanitize_index_references();
    let once = (
        model.docking_bays.iter().map(|bay| bay.path).collect::<Vec<_>>(),
        model.eye_points.iter().map(|eye| eye.attached_submodel).collect::<Vec<_>>(),
        model.glow_banks.iter().map(|bank| bank.model_parent).collect::<Vec<_>>(),
    );

    model.sanitize_index_references();
    let twice = (
        model.docking_bays.iter().map(|bay| bay.path).collect::<Vec<_>>(),
        model.eye_points.iter().map(|eye| eye.attached_submodel).collect::<Vec<_>>(),
        model.glow_banks.iter().map(|bank| bank.model_parent).collect::<Vec<_>>(),
    );

    assert_eq!(once, twice);
}
