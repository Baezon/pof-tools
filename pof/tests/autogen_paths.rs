//! Tests for auto-generated paths: the `PathTarget` primitives and the whole-model
//! `compute_auto_gen_paths` built on top of them.

use pof::*;

fn smodel(id: u32, name: &str, props: &str, offset: Vec3d, radius: f32) -> Submodel {
    Submodel {
        id: SubmodelId(id),
        name: name.to_string(),
        properties: props.to_string(),
        offset,
        radius,
        ..Default::default()
    }
}

/// detail0, one subsystem submodel (engine01), and one plain submodel which never gets a path.
fn base_model() -> Model {
    let mut model = Model::default();
    model.submodels = SubmodelVec(vec![
        smodel(0, "detail0", "", Vec3d::ZERO, 100.0),
        smodel(1, "engine01", "$special=subsystem", Vec3d::new(0.0, 0.0, -50.0), 10.0),
        smodel(2, "hull", "", Vec3d::new(0.0, 10.0, 0.0), 20.0),
    ]);
    model
}

/// base_model plus a turret whose base submodel is itself flagged as a subsystem.
fn turret_model() -> Model {
    let mut model = base_model();
    model.submodels.0.push(smodel(3, "turret01", "$special=subsystem", Vec3d::new(10.0, 0.0, 0.0), 5.0));
    model.submodels.0.push(smodel(4, "turret01-barrel", "", Vec3d::ZERO, 3.0));
    model.turrets.push(Turret {
        base_model: SubmodelId(3),
        gun_model: SubmodelId(4),
        normal: NormalVec3::try_from(Vec3d::new(1.0, 0.0, 0.0)).unwrap(),
        fire_points: vec![],
    });
    model
}

fn parents(paths: &[Path]) -> Vec<&str> {
    paths.iter().map(|path| path.parent.as_str()).collect()
}

// ---------------------------------------------------------------- whole model generation

#[test]
fn subsystem_submodel_gets_a_path() {
    let model = base_model();
    let (paths, docks) = model.compute_auto_gen_paths();
    assert!(docks.is_empty());
    assert_eq!(parents(&paths), vec!["engine01"], "only the subsystem submodel should get a path");
    assert_eq!(paths[0].name, "$path01");
    assert_eq!(paths[0].points.len(), 2);
}

#[test]
fn spaced_separator_is_still_a_subsystem() {
    let mut model = base_model();
    // FSO accepts these separators (get_user_prop_value skips whitespace, '=' and ':'),
    // so pof-tools has to as well
    model.submodels.0.push(smodel(3, "sensors", "$special: subsystem", Vec3d::new(0.0, 30.0, 0.0), 4.0));
    model.submodels.0.push(smodel(4, "comms", "$special = subsystem", Vec3d::new(0.0, -30.0, 0.0), 4.0));
    let (paths, _) = model.compute_auto_gen_paths();
    assert_eq!(parents(&paths), vec!["engine01", "sensors", "comms"]);
}

#[test]
fn existing_path_dedupes_case_insensitively() {
    let mut model = base_model();
    model.paths.push(Path { name: "$path01".into(), parent: "ENGINE01".into(), points: vec![] });
    let (paths, _) = model.compute_auto_gen_paths();
    assert!(paths.is_empty(), "a differently cased parent should still count as covered: {:?}", parents(&paths));
}

#[test]
fn special_point_dedupes_across_dollar_prefix() {
    let mut model = base_model();
    // name stored without the '$', as a dae import can produce; the path's parent has one
    model.special_points.push(SpecialPoint {
        name: "repair".into(),
        properties: "$special=subsystem".into(),
        position: Vec3d::new(0.0, 5.0, 0.0),
        radius: 2.0,
    });
    let (paths, _) = model.compute_auto_gen_paths();
    assert_eq!(paths.len(), 2, "engine01 + repair");

    model.paths.push(Path { name: "$path01".into(), parent: "$Repair".into(), points: vec![] });
    let (paths, _) = model.compute_auto_gen_paths();
    assert_eq!(parents(&paths), vec!["engine01"], "the special point should already be covered");
}

#[test]
fn dock_paths_are_named_and_assigned() {
    let mut model = base_model();
    model.docking_bays.push(Dock { position: Vec3d::new(0.0, 0.0, 20.0), ..Default::default() });
    let (paths, docks) = model.compute_auto_gen_paths();
    assert_eq!(paths.len(), 2, "engine01 then the dock");
    assert_eq!(paths[1].parent, "$dock01-01");
    assert_eq!(paths[1].points.len(), 4);
    assert_eq!(docks, vec![(0, PathId(1))], "dock 0 points at index 1 of the combined list");
}

#[test]
fn turret_beats_submodel_path() {
    let model = turret_model();
    let (paths, _) = model.compute_auto_gen_paths();
    assert_eq!(
        parents(&paths),
        vec!["turret01", "engine01"],
        "turret path first, and no second, wrongly shaped path for its base submodel"
    );
}

// ---------------------------------------------------------------- path targets

#[test]
fn path_targets_are_in_generation_order_and_skip_turret_bases() {
    let mut model = turret_model();
    model.special_points.push(SpecialPoint {
        name: "$repair".into(),
        properties: "$special=subsystem".into(),
        position: Vec3d::new(0.0, 5.0, 0.0),
        radius: 2.0,
    });
    model.special_points.push(SpecialPoint { name: "$decor".into(), properties: "".into(), position: Vec3d::ZERO, radius: 1.0 });
    model.docking_bays.push(Dock::default());

    assert_eq!(
        model.path_targets(),
        vec![
            PathTarget::Turret(0),
            PathTarget::Submodel(SubmodelId(1)), // engine01, but NOT turret01 (SubmodelId(3))
            PathTarget::SpecialPoint(0),         // $repair, but not the non-subsystem $decor
            PathTarget::DockingBay(0),
        ]
    );
}

#[test]
fn canonical_path_target_folds_a_turret_base_into_its_turret() {
    let model = turret_model();
    assert_eq!(model.canonical_path_target(PathTarget::Submodel(SubmodelId(3))), PathTarget::Turret(0));
    assert_eq!(model.canonical_path_target(PathTarget::Submodel(SubmodelId(1))), PathTarget::Submodel(SubmodelId(1)));
    assert_eq!(model.canonical_path_target(PathTarget::DockingBay(4)), PathTarget::DockingBay(4));
}

#[test]
fn gen_path_for_matches_what_the_whole_model_pass_produces() {
    let model = turret_model();
    let (paths, _) = model.compute_auto_gen_paths();

    for (path, target) in paths.iter().zip(model.path_targets()) {
        let single = model.gen_path_for(target, path.name.clone());
        assert_eq!(single.parent, path.parent);
        assert_eq!(single.points.len(), path.points.len());
        for (a, b) in single.points.iter().zip(&path.points) {
            assert_eq!(a.position, b.position, "{:?} position", target);
            assert_eq!(a.radius.to_bits(), b.radius.to_bits(), "{:?} radius", target);
        }
    }
}

// ---------------------------------------------------------------- reverse lookup

#[test]
fn path_target_finds_the_owning_object() {
    let mut model = turret_model();
    model.paths.push(Path { name: "$path01".into(), parent: "turret01".into(), points: vec![] });
    model.paths.push(Path { name: "$path02".into(), parent: "Engine01".into(), points: vec![] });
    model.paths.push(Path { name: "$path03".into(), parent: "nothing here".into(), points: vec![] });

    assert_eq!(model.path_target(&model.path_name_index(), PathId(0)), Some(PathTarget::Turret(0)));
    assert_eq!(model.path_target(&model.path_name_index(), PathId(1)), Some(PathTarget::Submodel(SubmodelId(1))));
    assert_eq!(model.path_target(&model.path_name_index(), PathId(2)), None, "an unmatched parent is legitimate, not an error");
    assert_eq!(model.path_target(&model.path_name_index(), PathId(99)), None, "a dangling id must not panic or claim a path");
}

#[test]
fn a_docks_index_link_wins_over_the_parent_name() {
    let mut model = turret_model();
    // a path which *says* it belongs to the turret, but which bay 0 actually links to
    model.paths.push(Path { name: "$path01".into(), parent: "turret01".into(), points: vec![] });
    model.docking_bays.push(Dock { path: Some(PathId(0)), ..Default::default() });

    assert_eq!(model.path_target(&model.path_name_index(), PathId(0)), Some(PathTarget::DockingBay(0)), "the index link is what FSO follows");
    assert_eq!(model.first_path_for(&model.path_name_index(), PathTarget::DockingBay(0)), Some(PathId(0)));
}

#[test]
fn extra_paths_for_one_target_are_all_reported_but_only_the_first_is_live() {
    let mut model = base_model();
    model.paths.push(Path { name: "$path01".into(), parent: "engine01".into(), points: vec![] });
    model.paths.push(Path { name: "$path02".into(), parent: "$ENGINE01".into(), points: vec![] });

    let target = PathTarget::Submodel(SubmodelId(1));
    assert!(model.path_claimants(&model.path_name_index(), PathId(0)).contains(&target));
    assert!(model.path_claimants(&model.path_name_index(), PathId(1)).contains(&target));
    // FSO's model_set_subsys_path_nums stops at the first match, so the second is dead weight
    assert_eq!(model.first_path_for(&model.path_name_index(), target), Some(PathId(0)));
}

// ---------------------------------------------------------------- claimants

#[test]
fn an_ordinary_path_has_exactly_one_claimant() {
    let mut model = turret_model();
    model.docking_bays.push(Dock { position: Vec3d::new(0.0, 0.0, 20.0), ..Default::default() });
    let (generated, assignments) = model.compute_auto_gen_paths();
    model.paths.extend(generated);
    for (bay, path) in assignments {
        model.docking_bays[bay].path = Some(path);
    }

    // a turret path names the base submodel, but the turret and its base are one object
    assert_eq!(model.path_claimants(&model.path_name_index(), PathId(0)), vec![PathTarget::Turret(0)]);
    assert_eq!(model.path_claimants(&model.path_name_index(), PathId(1)), vec![PathTarget::Submodel(SubmodelId(1))]);
    assert_eq!(model.path_claimants(&model.path_name_index(), PathId(2)), vec![PathTarget::DockingBay(0)]);
    assert!((0..3).all(|idx| !model.path_is_contested(&model.path_name_index(), PathId(idx))));

    assert!(model.path_claimants(&model.path_name_index(), PathId(99)).is_empty(), "a dangling id claims nothing");
}

#[test]
fn a_name_shared_by_two_objects_is_contested() {
    let mut model = base_model();
    model.submodels.0.push(smodel(3, "repair", "$special=subsystem", Vec3d::new(0.0, 10.0, 0.0), 5.0));
    model.special_points.push(SpecialPoint {
        name: "$repair".into(),
        properties: "$special=subsystem".into(),
        position: Vec3d::new(0.0, 5.0, 0.0),
        radius: 2.0,
    });
    let (generated, _) = model.compute_auto_gen_paths();
    model.paths.extend(generated);

    // one path between the two of them, which is all FSO would use, rather than one each
    assert_eq!(parents(&model.paths), vec!["engine01", "repair"]);
    assert!(model.compute_auto_gen_paths().0.is_empty(), "still idempotent");

    // both objects find it, so neither panel offers to append another
    let smodel_target = PathTarget::Submodel(SubmodelId(3));
    let spcl_target = PathTarget::SpecialPoint(0);
    assert_eq!(model.first_path_for(&model.path_name_index(), smodel_target), Some(PathId(1)));
    assert_eq!(model.first_path_for(&model.path_name_index(), spcl_target), Some(PathId(1)));

    // ...and they're both claiming it, so neither may regenerate it. The name collision is a real
    // problem with the model, and the user resolves it by renaming one of the two.
    assert_eq!(model.path_claimants(&model.path_name_index(), PathId(1)), vec![smodel_target, spcl_target]);
    assert!(model.path_is_contested(&model.path_name_index(), PathId(1)));
}

#[test]
fn a_decorative_object_lays_no_claim_to_a_subsystems_path() {
    let mut model = base_model();
    model.submodels.0.push(smodel(3, "sensors", "$special=subsystem", Vec3d::new(0.0, 30.0, 0.0), 4.0));
    // same name, but no $special=subsystem, so FSO never hands it a path
    model.special_points.push(SpecialPoint {
        name: "$sensors".into(),
        properties: "".into(),
        position: Vec3d::new(0.0, 5.0, 0.0),
        radius: 2.0,
    });
    let (generated, _) = model.compute_auto_gen_paths();
    model.paths.extend(generated);

    let sensors = PathId(1);
    assert_eq!(model.path_claimants(&model.path_name_index(), sensors), vec![PathTarget::Submodel(SubmodelId(3))]);
    assert!(!model.path_is_contested(&model.path_name_index(), sensors), "a decorative namesake must not deadlock the subsystem");
    assert_eq!(model.path_target(&model.path_name_index(), sensors), Some(PathTarget::Submodel(SubmodelId(3))), "and it keeps submodel geometry");
}

#[test]
fn docking_bays_sharing_a_path_dont_contest_it() {
    // several dockpoints using one approach path is ordinary, not a conflict
    let mut model = base_model();
    model.docking_bays.push(Dock { position: Vec3d::new(0.0, 0.0, 20.0), ..Default::default() });
    model.docking_bays.push(Dock { position: Vec3d::new(0.0, 0.0, -20.0), ..Default::default() });
    let (generated, _) = model.compute_auto_gen_paths();
    model.paths.extend(generated);

    // the user points both bays at bay 0's path
    model.docking_bays[0].path = Some(PathId(1));
    model.docking_bays[1].path = Some(PathId(1));

    assert_eq!(model.path_claimants(&model.path_name_index(), PathId(1)), vec![PathTarget::DockingBay(0), PathTarget::DockingBay(1)]);
    assert!(!model.path_is_contested(&model.path_name_index(), PathId(1)));
    // each bay has its path, so neither is offered another, and auto-gen leaves them be
    assert_eq!(model.first_path_for(&model.path_name_index(), PathTarget::DockingBay(0)), Some(PathId(1)));
    assert_eq!(model.first_path_for(&model.path_name_index(), PathTarget::DockingBay(1)), Some(PathId(1)));
    assert!(model.compute_auto_gen_paths().0.is_empty());
}

#[test]
fn bays_sharing_a_path_with_a_named_object_still_contest_it() {
    // the bays count as one claim between them, but that claim still clashes with a turret's
    let mut model = turret_model();
    let (generated, _) = model.compute_auto_gen_paths();
    model.paths.extend(generated);
    assert_eq!(model.paths[0].parent, "turret01");

    model.docking_bays.push(Dock { path: Some(PathId(0)), ..Default::default() });
    model.docking_bays.push(Dock { path: Some(PathId(0)), ..Default::default() });

    assert!(model.path_is_contested(&model.path_name_index(), PathId(0)));
}

#[test]
fn a_path_docking_bays_share_is_rebuilt_once_for_the_first_of_them() {
    let mut model = base_model();
    model.docking_bays.push(Dock { position: Vec3d::new(0.0, 0.0, 20.0), ..Default::default() });
    model.docking_bays.push(Dock { position: Vec3d::new(0.0, 0.0, -20.0), ..Default::default() });
    let (generated, _) = model.compute_auto_gen_paths();
    model.paths.extend(generated);
    model.docking_bays[0].path = Some(PathId(1));
    model.docking_bays[1].path = Some(PathId(1));
    model.paths[1].points.clear();

    for (path_id, path) in model.compute_regenerated_paths() {
        model.paths[path_id.0 as usize] = path;
    }

    // bay 0's shape, not bay 1's - rebuilding once per bay would leave whichever came last
    let bay0 = model.gen_path_for(PathTarget::DockingBay(0), String::new());
    let positions = |path: &Path| path.points.iter().map(|point| point.position).collect::<Vec<_>>();
    assert_eq!(positions(&model.paths[1]), positions(&bay0));
    let index = model.path_name_index();
    assert_eq!(model.path_target(&index, PathId(1)), Some(PathTarget::DockingBay(0)), "the same bay the per-path button rebuilds from");
}

#[test]
fn a_bay_linked_to_another_objects_path_is_contested() {
    let mut model = turret_model();
    let (generated, _) = model.compute_auto_gen_paths();
    model.paths.extend(generated);
    assert_eq!(model.paths[0].parent, "turret01");

    model.docking_bays.push(Dock { path: Some(PathId(0)), ..Default::default() });

    assert_eq!(model.path_claimants(&model.path_name_index(), PathId(0)), vec![PathTarget::Turret(0), PathTarget::DockingBay(0)]);
    assert!(model.path_is_contested(&model.path_name_index(), PathId(0)));

    // a path of its own settles it, since a $dockNN-01 parent names no object
    model.paths.push(Path { name: "$path09".into(), parent: "$dock01-01".into(), points: vec![] });
    let own_path = PathId(model.paths.len() as u32 - 1);
    model.docking_bays[0].path = Some(own_path);
    assert_eq!(model.path_claimants(&model.path_name_index(), own_path), vec![PathTarget::DockingBay(0)]);
    assert!(!model.path_is_contested(&model.path_name_index(), PathId(0)));
}

#[test]
fn two_objects_of_the_same_kind_sharing_a_name_are_contested() {
    let mut model = base_model();
    // a second submodel with engine01's name. Nothing stops a file doing this, and FSO hands both
    // of them the first path matching the name.
    model.submodels.0.push(smodel(3, "engine01", "$special=subsystem", Vec3d::new(0.0, 0.0, 50.0), 10.0));

    let (generated, _) = model.compute_auto_gen_paths();
    model.paths.extend(generated);

    // one path between the two of them, the same as when the two are of different kinds
    assert_eq!(parents(&model.paths), vec!["engine01"]);
    assert!(model.compute_auto_gen_paths().0.is_empty(), "and generating again adds nothing");

    let (first, second) = (PathTarget::Submodel(SubmodelId(1)), PathTarget::Submodel(SubmodelId(3)));
    assert_eq!(model.path_claimants(&model.path_name_index(), PathId(0)), vec![first, second]);
    assert!(model.path_is_contested(&model.path_name_index(), PathId(0)));
    // neither is left looking pathless, which would have the panel offer to append another
    assert_eq!(model.first_path_for(&model.path_name_index(), first), Some(PathId(0)));
    assert_eq!(model.first_path_for(&model.path_name_index(), second), Some(PathId(0)));
}

#[test]
fn generating_twice_over_adds_nothing_the_second_time() {
    // idempotency is the behavioural shape of the claimant invariant: if some object which can own a
    // path is missing from a generated path's claimants, auto-gen generates for it again, forever.
    let mut model = turret_model();
    model.submodels.0.push(smodel(5, "engine01", "$special=subsystem", Vec3d::new(0.0, 0.0, 60.0), 8.0));
    model.submodels.0.push(smodel(6, "$repair", "", Vec3d::new(0.0, 20.0, 0.0), 4.0));
    model.special_points.push(SpecialPoint {
        name: "$repair".into(),
        properties: "$special=subsystem".into(),
        position: Vec3d::new(0.0, 5.0, 0.0),
        radius: 2.0,
    });
    model.special_points.push(SpecialPoint {
        name: "repair".into(),
        properties: "$special=subsystem".into(),
        position: Vec3d::new(0.0, -5.0, 0.0),
        radius: 2.0,
    });
    model.special_points.push(SpecialPoint { name: "$turret01".into(), properties: "".into(), position: Vec3d::ZERO, radius: 1.0 });
    model.docking_bays.push(Dock { position: Vec3d::new(0.0, 0.0, 20.0), ..Default::default() });

    let (generated, assignments) = model.compute_auto_gen_paths();
    model.paths.extend(generated);
    for (bay, path) in assignments {
        model.docking_bays[bay].path = Some(path);
    }

    let (again, assignments) = model.compute_auto_gen_paths();
    assert!(
        again.is_empty() && assignments.is_empty(),
        "auto-gen is not idempotent, it would keep adding: {:?}",
        again.iter().map(|path| &path.parent).collect::<Vec<_>>()
    );
}

#[test]
fn everything_which_claims_a_path_is_something_which_could_own_one() {
    // the invariant the target resolution helpers exist to keep: name matching must apply the same
    // eligibility rule path_targets does. Every round of review so far has turned up a version of
    // this drifting, so assert it rather than aligning the two by hand.
    let mut model = turret_model();

    // decorative namesakes of every kind, none of which FSO would hand a path to
    model.submodels.0.push(smodel(5, "$repair", "", Vec3d::new(0.0, 20.0, 0.0), 4.0));
    model.special_points.push(SpecialPoint {
        name: "$repair".into(),
        properties: "$special=subsystem".into(),
        position: Vec3d::new(0.0, 5.0, 0.0),
        radius: 2.0,
    });
    model.special_points.push(SpecialPoint {
        name: "$engine01".into(),
        properties: "".into(),
        position: Vec3d::new(0.0, -5.0, 0.0),
        radius: 2.0,
    });
    model.special_points.push(SpecialPoint { name: "$turret01".into(), properties: "".into(), position: Vec3d::ZERO, radius: 1.0 });
    model.docking_bays.push(Dock { position: Vec3d::new(0.0, 0.0, 20.0), ..Default::default() });

    let (generated, assignments) = model.compute_auto_gen_paths();
    model.paths.extend(generated);
    for (bay, path) in assignments {
        model.docking_bays[bay].path = Some(path);
    }
    // and a hand authored path naming something decorative
    model.paths.push(Path { name: "$path09".into(), parent: "$engine01".into(), points: vec![] });

    let targets = model.path_targets();
    for idx in 0..model.paths.len() {
        for claimant in model.path_claimants(&model.path_name_index(), PathId(idx as u32)) {
            assert!(model.can_own_a_path(claimant), "path {} claimed by {:?} which cannot own one", idx, claimant);
            assert!(targets.contains(&claimant), "path {} claimed by {:?} which path_targets does not yield", idx, claimant);
        }
    }

    // the decorative namesakes leave the real subsystems alone
    assert!(!model.path_is_contested(&model.path_name_index(), PathId(0)), "turret01");
    assert!(!model.path_is_contested(&model.path_name_index(), PathId(1)), "engine01");
}

// ---------------------------------------------------------------- name allocation

#[test]
fn only_path_style_names_count_towards_the_next_number() {
    let named = |names: &[&str]| {
        let mut model = base_model();
        model.paths = names.iter().map(|name| Path { name: name.to_string(), parent: "".into(), points: vec![] }).collect();
        model.next_path_number()
    };
    assert_eq!(named(&[]), 1);
    assert_eq!(named(&["$path05", "$Path12"]), 13);
    assert_eq!(named(&["$path", "$dock01-01", "hangar approach"]), 1);
}

#[test]
fn generated_names_continue_past_existing_ones() {
    let mut model = base_model();
    model.paths.push(Path { name: "$path03".into(), parent: "".into(), points: vec![] });
    model.paths.push(Path { name: "$Path07".into(), parent: "".into(), points: vec![] });
    model.paths.push(Path { name: "hand named".into(), parent: "".into(), points: vec![] });
    model.docking_bays.push(Dock::default());
    model.docking_bays.push(Dock::default());

    let names: Vec<String> = model.compute_auto_gen_paths().0.into_iter().map(|path| path.name).collect();
    assert_eq!(names, vec!["$path08", "$path09", "$path10"]);
}

#[test]
fn path_names_are_zero_padded_from_one() {
    let mut model = base_model();
    assert_eq!(model.compute_auto_gen_paths().0[0].name, "$path01");

    model.paths.push(Path { name: "$path99".into(), parent: "".into(), points: vec![] });
    assert_eq!(model.compute_auto_gen_paths().0[0].name, "$path100");
}

#[test]
fn name_matching_ignores_case_and_a_leading_dollar() {
    let same = |a: &str, b: &str| normalized_path_name(a) == normalized_path_name(b);
    assert!(same("$Repair", "repair"));
    assert!(same("TURRET01", "turret01"));
    assert!(same("$engine", "$ENGINE"));
    assert!(!same("turret01", "turret02"));
    assert!(!same("$dock01-01", "dock01"));
}

// ---------------------------------------------------------------- regeneration in place

#[test]
fn take_geometry_from_keeps_the_name_and_the_turret_assignments() {
    let mut path = Path {
        name: "hangar approach".into(),
        parent: "old parent".into(),
        points: vec![
            PathPoint { position: Vec3d::ZERO, radius: 1.0, turrets: vec![SubmodelId(3)] },
            PathPoint { position: Vec3d::ZERO, radius: 2.0, turrets: vec![SubmodelId(4), SubmodelId(5)] },
            PathPoint { position: Vec3d::ZERO, radius: 3.0, turrets: vec![SubmodelId(6)] },
        ],
    };
    let generated = Path {
        name: "$path01".into(),
        parent: "turret01".into(),
        points: vec![
            PathPoint { position: Vec3d::new(1.0, 0.0, 0.0), radius: 10.0, turrets: vec![] },
            PathPoint { position: Vec3d::new(2.0, 0.0, 0.0), radius: 20.0, turrets: vec![] },
        ],
    };

    path.take_geometry_from(generated);

    assert_eq!(path.name, "hangar approach", "a hand chosen name survives regeneration");
    assert_eq!(path.parent, "turret01");
    assert_eq!(path.points.len(), 2, "the surplus point is dropped");
    assert_eq!(path.points[0].position, Vec3d::new(1.0, 0.0, 0.0));
    assert_eq!(path.points[0].radius, 10.0);
    // these aren't editable in the GUI but do round trip through the file, so they must survive
    assert_eq!(path.points[0].turrets, vec![SubmodelId(3)]);
    assert_eq!(path.points[1].turrets, vec![SubmodelId(4), SubmodelId(5)]);
}

#[test]
fn take_geometry_from_handles_gaining_points() {
    let mut path = Path {
        name: "$path01".into(),
        parent: "engine01".into(),
        points: vec![PathPoint { position: Vec3d::ZERO, radius: 1.0, turrets: vec![SubmodelId(3)] }],
    };
    let mut model = base_model();
    model.docking_bays.push(Dock::default());
    path.take_geometry_from(model.gen_path_for(PathTarget::DockingBay(0), String::new()));

    assert_eq!(path.name, "$path01");
    assert_eq!(path.points.len(), 4);
    assert_eq!(path.points[0].turrets, vec![SubmodelId(3)]);
    assert!(path.points[1].turrets.is_empty());
}

// ---------------------------------------------------------------- docking bay path conflicts

#[test]
fn regenerating_every_path_restores_the_generated_geometry() {
    let mut model = turret_model();
    model.docking_bays.push(Dock { position: Vec3d::new(0.0, 0.0, 20.0), ..Default::default() });

    // do a whole-model auto-gen first, the way the dialog would
    let (generated, dock_assignments) = model.compute_auto_gen_paths();
    model.paths.extend(generated);
    for (bay_idx, path_id) in dock_assignments {
        model.docking_bays[bay_idx].path = Some(path_id);
    }
    assert_eq!(model.paths.len(), 3, "turret01, engine01, dock");
    let pristine = model.paths.clone();

    // a second path naming engine01, which FSO ignores and regeneration must not touch
    model.paths.push(Path { name: "spare".into(), parent: "$ENGINE01".into(), points: vec![] });

    // now mangle the generated ones, as a user editing points by hand would
    model.paths[0].name = "renamed by hand".into();
    model.paths[0].points[0].position = Vec3d::new(999.0, 999.0, 999.0);
    model.paths[1].points.clear();
    model.paths[2].points[3].radius = 42.0;

    for (path_id, path) in model.compute_regenerated_paths() {
        model.paths[path_id.0 as usize] = path;
    }

    for (idx, original) in pristine.iter().enumerate() {
        assert_eq!(model.paths[idx].parent, original.parent, "path {} parent", idx);
        assert_eq!(model.paths[idx].points.len(), original.points.len(), "path {} point count", idx);
        for (a, b) in model.paths[idx].points.iter().zip(&original.points) {
            assert_eq!(a.position, b.position, "path {} position", idx);
            assert_eq!(a.radius.to_bits(), b.radius.to_bits(), "path {} radius", idx);
        }
    }
    assert_eq!(model.paths[0].name, "renamed by hand", "a hand chosen name survives");
    assert_eq!(model.paths[3].name, "spare");
    assert!(model.paths[3].points.is_empty(), "the extra path naming engine01 is left alone");
}

#[test]
fn regenerating_freshly_generated_paths_changes_nothing() {
    let mut model = turret_model();
    model.docking_bays.push(Dock::default());
    let (generated, assignments) = model.compute_auto_gen_paths();
    model.paths.extend(generated);
    for (bay_idx, path_id) in assignments {
        model.docking_bays[bay_idx].path = Some(path_id);
    }
    // so the dialog records no undo step for a model with nothing to do
    assert!(model.compute_regenerated_paths().is_empty());
}

#[test]
fn an_empty_parent_names_nothing() {
    // every path in a POF older than version 20.02 has an empty parent, since the field isn't
    // written before then, and FSO doesn't resolve those either
    let mut model = base_model();
    model.submodels.0.push(smodel(3, "", "$special=subsystem", Vec3d::new(0.0, 20.0, 0.0), 4.0));
    model.paths.push(Path { name: "$path01".into(), parent: String::new(), points: vec![] });
    model.paths.push(Path { name: "$path02".into(), parent: "$".into(), points: vec![] });

    for idx in 0..2 {
        assert!(model.path_claimants(&model.path_name_index(), PathId(idx)).is_empty(), "path {} claims nothing", idx);
        assert!(!model.path_is_contested(&model.path_name_index(), PathId(idx)));
        assert_eq!(model.path_target(&model.path_name_index(), PathId(idx)), None);
    }

    // an object with no name can't be pointed at by a parent, so it isn't a target at all - offering
    // to generate for it would generate again on every run
    assert!(!model.can_own_a_path(PathTarget::Submodel(SubmodelId(3))));
    assert!(!model.path_targets().contains(&PathTarget::Submodel(SubmodelId(3))));

    let (generated, _) = model.compute_auto_gen_paths();
    model.paths.extend(generated);
    assert_eq!(parents(&model.paths).last(), Some(&"engine01"));
    assert!(model.compute_auto_gen_paths().0.is_empty(), "still idempotent with an unnamed subsystem about");
}

#[test]
fn nothing_unnamed_can_own_a_path() {
    // whatever the kind, an object with no name can't be pointed at by a path's parent, so it can't
    // own one - and generating for it anyway would generate again on every run. The panels ask this
    // predicate rather than checking is_subsystem themselves, so the two can't disagree.
    let mut model = base_model();
    model.submodels.0.push(smodel(3, "", "$special=subsystem", Vec3d::new(0.0, 20.0, 0.0), 4.0));
    model.submodels.0.push(smodel(4, "", "", Vec3d::new(10.0, 0.0, 0.0), 5.0)); // an unnamed turret base
    model.submodels.0.push(smodel(5, "barrel", "", Vec3d::ZERO, 3.0));
    model.turrets.push(Turret {
        base_model: SubmodelId(4),
        gun_model: SubmodelId(5),
        normal: NormalVec3::try_from(Vec3d::new(1.0, 0.0, 0.0)).unwrap(),
        fire_points: vec![],
    });
    model.special_points.push(SpecialPoint {
        name: String::new(),
        properties: "$special=subsystem".into(),
        position: Vec3d::new(0.0, 5.0, 0.0),
        radius: 2.0,
    });

    assert!(!model.can_own_a_path(PathTarget::Turret(0)), "turret with an unnamed base");
    assert!(!model.can_own_a_path(PathTarget::Submodel(SubmodelId(3))), "unnamed subsystem submodel");
    assert!(!model.can_own_a_path(PathTarget::SpecialPoint(0)), "unnamed subsystem special point");
    assert!(model.can_own_a_path(PathTarget::Submodel(SubmodelId(1))), "engine01 still can");

    assert_eq!(model.path_targets(), vec![PathTarget::Submodel(SubmodelId(1))]);

    let (generated, _) = model.compute_auto_gen_paths();
    model.paths.extend(generated);
    assert_eq!(parents(&model.paths), vec!["engine01"]);
    assert!(model.compute_auto_gen_paths().0.is_empty(), "and it stays idempotent with unnamed objects about");
}

/// Mirrors what deleting a submodel does to the path list: drop the paths named after it, then tell
/// the docking bays which indices went away.
fn delete_paths_named(model: &mut Model, doomed_name: &str) {
    let doomed: Vec<PathId> = model
        .paths
        .iter()
        .enumerate()
        .filter(|(_, path)| path.name == doomed_name)
        .map(|(idx, _)| PathId(idx as u32))
        .collect();

    model.paths.retain(|path| path.name != doomed_name);
    for &removed in doomed.iter().rev() {
        model.path_removal_fixup(removed);
    }
}

#[test]
fn dropping_paths_keeps_docking_bay_links_pointing_at_the_right_path() {
    let mut model = base_model();
    model.paths.push(Path { name: "$turret01".into(), parent: "turret01".into(), points: vec![] });
    model.paths.push(Path { name: "$path02".into(), parent: "$dock01-01".into(), points: vec![] });
    model.paths.push(Path { name: "$path03".into(), parent: "$dock02-01".into(), points: vec![] });

    model.docking_bays.push(Dock { path: Some(PathId(1)), ..Default::default() });
    model.docking_bays.push(Dock { path: Some(PathId(2)), ..Default::default() });
    // a bay pointing at the path which is about to go
    model.docking_bays.push(Dock { path: Some(PathId(0)), ..Default::default() });

    delete_paths_named(&mut model, "$turret01");

    assert_eq!(parents(&model.paths), vec!["$dock01-01", "$dock02-01"]);
    assert_eq!(model.docking_bays[0].path, Some(PathId(0)), "shifted down with its path");
    assert_eq!(model.docking_bays[1].path, Some(PathId(1)), "shifted down with its path");
    assert_eq!(model.docking_bays[2].path, None, "its path is gone, so it has none");

    // and the claim model agrees, rather than reading a stale index as an unclaimed bay
    assert_eq!(model.first_path_for(&model.path_name_index(), PathTarget::DockingBay(0)), Some(PathId(0)));
    assert_eq!(model.first_path_for(&model.path_name_index(), PathTarget::DockingBay(1)), Some(PathId(1)));
    assert!(model.compute_auto_gen_paths().0.iter().all(|path| path.parent != "$dock01-01"), "no duplicate for bay 0");
}

#[test]
fn deleting_a_submodel_takes_its_own_path_and_not_its_namesake() {
    let mut model = base_model();
    let (generated, _) = model.compute_auto_gen_paths();
    model.paths.extend(generated);
    // an unrelated dock path which merely happens to be named after the submodel
    model.paths.push(Path { name: "$engine01".into(), parent: "$dock01-01".into(), points: vec![] });

    let engine01 = PathTarget::Submodel(SubmodelId(1));
    assert_eq!(parents(&model.paths), vec!["engine01", "$dock01-01"]);
    assert_eq!(
        model.paths_claimed_only_by(&[engine01]),
        vec![PathId(0)],
        "its own path, not the one called $engine01"
    );

    // a turret base resolves to its turret, whose path names the base
    let mut model = turret_model();
    let (generated, _) = model.compute_auto_gen_paths();
    model.paths.extend(generated);
    assert_eq!(model.paths_claimed_only_by(&[PathTarget::Submodel(SubmodelId(3))]), vec![PathId(0)], "turret01's path");

    // a path two objects answer to is left alone, rather than stranding the survivor
    let mut model = base_model();
    model.submodels.0.push(smodel(3, "engine01", "$special=subsystem", Vec3d::new(0.0, 0.0, 60.0), 8.0));
    let (generated, _) = model.compute_auto_gen_paths();
    model.paths.extend(generated);
    assert!(model.path_is_contested(&model.path_name_index(), PathId(0)));
    assert!(model.paths_claimed_only_by(&[PathTarget::Submodel(SubmodelId(1))]).is_empty(), "shared, so not solely its own");
}

#[test]
fn deleting_a_submodel_takes_the_paths_of_the_bays_going_with_it() {
    // the bays whose $parent_submodel names a submodel are deleted along with it, so the panel hands
    // those over too - and their paths must go rather than be left behind, claimed by nothing
    let mut model = base_model();
    model.docking_bays.push(Dock {
        properties: "$parent_submodel=engine01".into(),
        position: Vec3d::new(0.0, 0.0, 20.0),
        ..Default::default()
    });
    model.docking_bays.push(Dock {
        properties: "$parent_submodel=hull".into(),
        position: Vec3d::new(0.0, 0.0, -20.0),
        ..Default::default()
    });
    let (generated, assignments) = model.compute_auto_gen_paths();
    model.paths.extend(generated);
    for (bay, path) in assignments {
        model.docking_bays[bay].path = Some(path);
    }
    assert_eq!(parents(&model.paths), vec!["engine01", "$dock01-01", "$dock02-01"]);

    let doomed = [PathTarget::Submodel(SubmodelId(1)), PathTarget::DockingBay(0)];
    assert_eq!(model.paths_claimed_only_by(&doomed), vec![PathId(0), PathId(1)], "not the hull bay's path");

    // a path claimed only by things which are all going goes too, even when there are two of them
    model.docking_bays[0].path = Some(PathId(0));
    assert_eq!(model.paths_claimed_only_by(&doomed), vec![PathId(0)], "path 1 is claimed by nothing now");

    // ...but one which something staying behind also claims is left for it
    model.docking_bays[1].path = Some(PathId(0));
    assert!(model.paths_claimed_only_by(&doomed).is_empty(), "the hull bay still needs it");
}
