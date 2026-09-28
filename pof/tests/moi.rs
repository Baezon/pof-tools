//! Tests for the mass properties worked out from a model's geometry: the moment of inertia, which
//! the header stores inverted, the center of mass, and the surface centroid behind the visual center.

use nalgebra_glm as glm;
use pof::*;

type Box3 = ([f32; 3], [f32; 3]);

const SOLID: MassModel = MassModel::Solid { cap_flat_holes: false, skip_open: false };
const SOLID_SKIPPING: MassModel = MassModel::Solid { cap_flat_holes: false, skip_open: true };
const SOLID_CAPPING: MassModel = MassModel::Solid { cap_flat_holes: true, skip_open: false };
const SOLID_CAPPING_AND_SKIPPING: MassModel = MassModel::Solid { cap_flat_holes: true, skip_open: true };
const SHELL: MassModel = MassModel::Shell;

/// The faces of a box whose corners are numbered by which of x, y and z are at their maximum,
/// wound to face outwards.
const FACES: [[u32; 4]; 6] = [[4, 6, 2, 0], [3, 7, 5, 1], [1, 5, 4, 0], [6, 7, 3, 2], [2, 3, 1, 0], [5, 7, 6, 4]];

fn mesh_of(verts: Vec<Vec3d>, faces: Vec<Vec<u32>>) -> BspData {
    let polygons = faces.into_iter().map(|face| Polygon {
        normal: Vec3d::ZERO,
        texture: TextureId(0),
        verts: face
            .into_iter()
            .map(|i| PolyVertex {
                vertex_id: VertexId(i),
                normal_id: NormalId(0),
                uv: (0.0, 0.0),
            })
            .collect(),
    });
    let collision_tree = BspData::recalculate(&verts, polygons);
    BspData {
        verts,
        norms: vec![Vec3d::new(0.0, 1.0, 0.0)],
        collision_tree,
    }
}

fn corners((min, max): &Box3) -> impl Iterator<Item = Vec3d> + '_ {
    (0..8).map(move |i| {
        let pick = |axis: usize| if i & (1 << axis) == 0 { min[axis] } else { max[axis] };
        Vec3d::new(pick(0), pick(1), pick(2))
    })
}

/// The boxes as one mesh, each a closed shell of six quads, or of twelve triangles.
/// A box with its min and max the wrong way around on one axis comes out facing inwards.
fn mesh(boxes: &[Box3], triangulate: bool) -> BspData {
    let mut verts = vec![];
    let mut faces = vec![];
    for a_box in boxes {
        let first = verts.len() as u32;
        verts.extend(corners(a_box));
        for face in FACES {
            let [a, b, c, d] = face.map(|i| first + i);
            if triangulate {
                faces.extend([vec![a, b, c], vec![a, c, d]]);
            } else {
                faces.push(vec![a, b, c, d]);
            }
        }
    }
    mesh_of(verts, faces)
}

/// A box with some of its faces missing. They're numbered in the order of x, y and z, the minimum before the maximum.
fn box_without(a_box: &Box3, missing: &[usize]) -> BspData {
    let faces = FACES
        .iter()
        .enumerate()
        .filter(|(i, _)| !missing.contains(i))
        .map(|(_, face)| face.to_vec());
    mesh_of(corners(a_box).collect(), faces.collect())
}

/// A box that shares no vert between its faces, each having four of its own.
fn unwelded_mesh(a_box: &Box3) -> BspData {
    let corners: Vec<_> = corners(a_box).collect();
    let verts = FACES.iter().flatten().map(|&i| corners[i as usize]).collect();
    let faces = (0..6).map(|i| (i * 4..i * 4 + 4).collect()).collect();
    mesh_of(verts, faces)
}

fn submodel(name: &str, parent: Option<u32>, offset: [f32; 3], boxes: &[Box3]) -> Submodel {
    Submodel {
        name: name.to_string(),
        parent: parent.map(SubmodelId),
        offset: offset.into(),
        bsp_data: mesh(boxes, false),
        ..Default::default()
    }
}

/// The same submodel with a polygon missing.
fn opened(mut smodel: Submodel) -> Submodel {
    let polygons = std::mem::take(&mut smodel.bsp_data.collision_tree)
        .into_leaves()
        .map(|(_, poly)| poly)
        .skip(1);
    smodel.bsp_data.collision_tree = BspData::recalculate(&smodel.bsp_data.verts, polygons);
    smodel
}

/// A model of these submodels, the first of them being detail0.
fn model_of(submodels: Vec<Submodel>, mass: f32) -> Model {
    let mut model = Model::default();
    model.submodels = SubmodelVec(submodels);
    for (i, smodel) in model.submodels.0.iter_mut().enumerate() {
        smodel.id = SubmodelId(i as u32);
    }
    model.header.detail_levels = vec![SubmodelId(0)];
    model.header.mass = mass;
    model.recalc_all_children_ids();
    model.recalc_semantic_name_links();
    model
}

/// A hull with a turret hanging off it, lopsided enough that no part of the tensor is zero.
fn lopsided_model(mass_model: MassModel) -> Model {
    let mut model = model_of(
        vec![
            submodel("detail0", None, [0.0; 3], &[([-3.0, -1.0, -6.0], [2.0, 1.5, 5.0])]),
            submodel("turret01", Some(0), [1.5, 2.0, -3.0], &[([-0.5, -0.5, -1.0], [1.0, 0.5, 2.0])]),
        ],
        250.0,
    );
    model.header.moment_of_inertia = moi(&model, mass_model);
    model
}

fn moi(model: &Model, mass_model: MassModel) -> Mat3d {
    model.recalc_moi(mass_model).unwrap().0
}

fn center_of_mass(model: &Model, mass_model: MassModel) -> Vec3d {
    model.recalc_center_of_mass(mass_model).unwrap().0
}

fn bad_mesh(open: &[u32], inside_out: &[u32]) -> MassPropertiesError {
    MassPropertiesError::BadMesh {
        open: open.iter().copied().map(SubmodelId).collect(),
        inside_out: inside_out.iter().copied().map(SubmodelId).collect(),
    }
}

fn diagonal(x: f32, y: f32, z: f32) -> Mat3d {
    Mat3d {
        rvec: Vec3d::new(x, 0.0, 0.0),
        uvec: Vec3d::new(0.0, y, 0.0),
        fvec: Vec3d::new(0.0, 0.0, z),
    }
}

/// The tensor's entries run over many orders of magnitude, and those off the diagonal are often
/// zero, so they're held to a tolerance set by the largest of them.
#[track_caller]
fn assert_close(actual: Mat3d, expected: Mat3d) {
    let actual = glm::Mat3x3::from(actual);
    let expected = glm::Mat3x3::from(expected);
    let scale = expected.iter().fold(0.0, |max: f32, val| max.max(val.abs()));
    assert!(scale > 0.0, "expected a tensor, got all zeroes");
    for (actual_val, expected_val) in actual.iter().zip(expected.iter()) {
        assert!((actual_val - expected_val).abs() <= 1e-4 * scale, "\n{}\nisn't\n{}", actual, expected);
    }
}

#[track_caller]
fn assert_vec_close(actual: Vec3d, expected: Vec3d) {
    assert!((actual - expected).magnitude() <= 1e-4, "{:?} isn't {:?}", actual, expected);
}

// ---------------------------------------------------------------- moment of inertia

#[test]
fn a_cube_has_the_textbook_tensor() {
    let model = model_of(vec![submodel("detail0", None, [0.0; 3], &[([-1.0; 3], [1.0; 3])])], 100.0);

    // 1/6 m L^2 about each axis when solid, and 5/18 m L^2 when hollow
    let solid = 6.0 / (100.0 * 2.0 * 2.0);
    assert_close(moi(&model, SOLID), diagonal(solid, solid, solid));
    let hollow = 18.0 / (5.0 * 100.0 * 2.0 * 2.0);
    assert_close(moi(&model, SHELL), diagonal(hollow, hollow, hollow));
}

#[test]
fn how_finely_a_face_is_cut_up_makes_no_difference() {
    let boxes = [([1.0, 2.0, -1.0], [4.0, 3.0, 5.0])];
    let quads = model_of(vec![submodel("detail0", None, [0.0; 3], &boxes)], 40.0);
    let mut triangles = model_of(vec![submodel("detail0", None, [0.0; 3], &boxes)], 40.0);
    triangles.submodels.0[0].bsp_data = mesh(&boxes, true);

    for mass_model in [SOLID, SHELL] {
        assert_close(moi(&triangles, mass_model), moi(&quads, mass_model));
    }
}

#[test]
fn a_submodel_counts_from_where_its_offsets_put_it() {
    let nested = model_of(
        vec![
            submodel("detail0", None, [0.0; 3], &[([-2.0, -1.0, -4.0], [2.0, 1.0, 4.0])]),
            submodel("turret01", Some(0), [3.0, 1.0, -2.0], &[([-1.0, 0.0, -1.0], [1.0, 1.0, 1.0])]),
            submodel("turret01-arm", Some(1), [0.0, 2.0, 0.5], &[([-0.5, 0.0, 0.0], [0.5, 0.5, 3.0])]),
        ],
        75.0,
    );
    let baked = model_of(
        vec![submodel(
            "detail0",
            None,
            [0.0; 3],
            &[
                ([-2.0, -1.0, -4.0], [2.0, 1.0, 4.0]),
                ([2.0, 1.0, -3.0], [4.0, 2.0, -1.0]),
                ([2.5, 3.0, -1.5], [3.5, 3.5, 1.5]),
            ],
        )],
        75.0,
    );

    for mass_model in [SOLID, SHELL] {
        assert_close(moi(&nested, mass_model), moi(&baked, mass_model));
        assert_vec_close(center_of_mass(&nested, mass_model), center_of_mass(&baked, mass_model));
    }
    let (nested_area, nested_center) = nested.surface_area_average_pos();
    let (baked_area, baked_center) = baked.surface_area_average_pos();
    assert!((nested_area - baked_area).abs() <= 1e-4 * baked_area);
    assert_vec_close(nested_center, baked_center);
}

#[test]
fn what_an_intact_ship_doesnt_show_is_left_out() {
    let hull = submodel("detail0", None, [0.0; 3], &[([-2.0, -1.0, -4.0], [2.0, 1.0, 4.0])]);
    let turret = submodel("turret01", Some(0), [3.0, 1.0, -2.0], &[([-1.0, 0.0, -1.0], [1.0, 1.0, 1.0])]);
    let intact = model_of(vec![hull.clone(), turret.clone()], 75.0);
    let with_wreckage = model_of(
        vec![
            hull,
            turret,
            opened(submodel("turret01-destroyed", Some(0), [3.0, 1.0, -2.0], &[([-1.0, 0.0, -1.0], [1.0, 0.5, 1.0])])),
            submodel("stump", Some(2), [0.0, 0.5, 0.0], &[([-0.2, 0.0, -0.2], [0.2, 1.0, 0.2])]),
            submodel("debris-turret01", Some(0), [5.0, 0.0, 0.0], &[([-1.0; 3], [1.0; 3])]),
        ],
        75.0,
    );

    for mass_model in [SOLID, SHELL] {
        assert_close(moi(&with_wreckage, mass_model), moi(&intact, mass_model));
        assert_vec_close(center_of_mass(&with_wreckage, mass_model), center_of_mass(&intact, mass_model));
    }
    assert_vec_close(with_wreckage.surface_area_average_pos().1, intact.surface_area_average_pos().1);
}

#[test]
fn a_lower_detail_level_is_left_out() {
    let hull = submodel("detail0", None, [0.0; 3], &[([-2.0, -1.0, -4.0], [2.0, 1.0, 4.0])]);
    let alone = model_of(vec![hull.clone()], 75.0);
    let with_lod = model_of(vec![hull, opened(submodel("detail1", None, [0.0; 3], &[([-9.0; 3], [9.0; 3])]))], 75.0);

    for mass_model in [SOLID, SHELL] {
        assert_close(moi(&with_lod, mass_model), moi(&alone, mass_model));
    }
}

#[test]
fn a_model_with_nothing_to_weigh_has_no_tensor() {
    let no_geometry = model_of(vec![submodel("detail0", None, [0.0; 3], &[])], 100.0);
    let cube = [([-1.0; 3], [1.0; 3])];

    for mass_model in [SOLID, SOLID_SKIPPING, SHELL] {
        assert_eq!(Model::default().recalc_moi(mass_model).err(), Some(MassPropertiesError::NothingToWeigh));
        assert_eq!(no_geometry.recalc_moi(mass_model).err(), Some(MassPropertiesError::NothingToWeigh));
        assert_eq!(no_geometry.recalc_center_of_mass(mass_model).err(), Some(MassPropertiesError::NothingToWeigh));

        for mass in [0.0, -5.0, f32::NAN, f32::INFINITY] {
            let model = model_of(vec![submodel("detail0", None, [0.0; 3], &cube)], mass);
            assert_eq!(model.recalc_moi(mass_model).err(), Some(MassPropertiesError::InvalidMass), "mass {}", mass);
            assert_vec_close(center_of_mass(&model, mass_model), Vec3d::ZERO);
        }
    }
}

#[test]
fn a_polygon_short_of_three_verts_is_passed_over() {
    let mut model = model_of(vec![submodel("detail0", None, [0.0; 3], &[([-1.0; 3], [1.0; 3])])], 100.0);
    let bsp_data = &mut model.submodels.0[0].bsp_data;
    for len in 0..3 {
        let poly = Polygon {
            normal: Vec3d::ZERO,
            texture: TextureId(0),
            verts: (0..len)
                .map(|i| PolyVertex {
                    vertex_id: VertexId(i),
                    normal_id: NormalId(0),
                    uv: (0.0, 0.0),
                })
                .collect(),
        };
        bsp_data.collision_tree = BspNode::Split {
            bbox: BoundingBox::ZERO,
            front: Box::new(std::mem::take(&mut bsp_data.collision_tree)),
            back: Box::new(BspNode::Leaf { bbox: BoundingBox::ZERO, poly }),
        };
    }

    let cube = model_of(vec![submodel("detail0", None, [0.0; 3], &[([-1.0; 3], [1.0; 3])])], 100.0);
    for mass_model in [SOLID, SHELL] {
        assert_close(moi(&model, mass_model), moi(&cube, mass_model));
    }
    assert_vec_close(model.surface_area_average_pos().1, Vec3d::ZERO);
}

// ---------------------------------------------------------------- what a solid asks of the mesh

#[test]
fn a_mesh_with_a_hole_in_it_cant_be_solid() {
    let model = model_of(vec![opened(submodel("detail0", None, [0.0; 3], &[([-1.0; 3], [1.0; 3])]))], 100.0);

    assert_eq!(model.recalc_moi(SOLID).err(), Some(bad_mesh(&[0], &[])));
    assert_eq!(model.recalc_center_of_mass(SOLID).err(), Some(bad_mesh(&[0], &[])));
    assert!(model.recalc_moi(SHELL).is_ok());
}

#[test]
fn a_mesh_facing_inwards_cant_be_solid() {
    let model = model_of(vec![submodel("detail0", None, [0.0; 3], &[([1.0, -1.0, -1.0], [-1.0, 1.0, 1.0])])], 100.0);

    assert_eq!(model.recalc_moi(SOLID).err(), Some(bad_mesh(&[], &[0])));
    assert_eq!(model.recalc_moi(SOLID_SKIPPING).err(), Some(bad_mesh(&[], &[0])));
    assert!(model.recalc_moi(SHELL).is_ok());
}

#[test]
fn every_unfit_submodel_is_named_at_once() {
    let model = model_of(
        vec![
            submodel("detail0", None, [0.0; 3], &[([-2.0, -1.0, -4.0], [2.0, 1.0, 4.0])]),
            opened(submodel("turret01", Some(0), [3.0, 1.0, -2.0], &[([-1.0, 0.0, -1.0], [1.0, 1.0, 1.0])])),
            submodel("pod", Some(0), [-3.0, 0.0, 0.0], &[([1.0, -1.0, -1.0], [-1.0, 1.0, 1.0])]),
            opened(submodel("turret02", Some(0), [3.0, 1.0, 2.0], &[([-1.0, 0.0, -1.0], [1.0, 1.0, 1.0])])),
        ],
        75.0,
    );

    assert_eq!(model.recalc_moi(SOLID).err(), Some(bad_mesh(&[1, 3], &[2])));
    // the open ones would only have been skipped
    assert_eq!(model.recalc_moi(SOLID_SKIPPING).err(), Some(bad_mesh(&[], &[2])));
}

#[test]
fn the_open_submodels_can_be_skipped_on_request() {
    let hull = submodel("detail0", None, [0.0; 3], &[([-2.0, -1.0, -4.0], [2.0, 1.0, 4.0])]);
    let alone = model_of(vec![hull.clone()], 75.0);
    let with_turret = model_of(
        vec![
            hull,
            opened(submodel("turret01", Some(0), [3.0, 1.0, -2.0], &[([-1.0, 0.0, -1.0], [1.0, 1.0, 1.0])])),
        ],
        75.0,
    );

    let (tensor, skipped) = with_turret.recalc_moi(SOLID_SKIPPING).unwrap();
    assert_close(tensor, moi(&alone, SOLID));
    assert_eq!(skipped, [SubmodelId(1)]);

    let (center, skipped) = with_turret.recalc_center_of_mass(SOLID_SKIPPING).unwrap();
    assert_vec_close(center, center_of_mass(&alone, SOLID));
    assert_eq!(skipped, [SubmodelId(1)]);

    assert_eq!(alone.recalc_moi(SOLID_SKIPPING).unwrap().1, []);
}

#[test]
fn skipping_every_submodel_leaves_nothing_to_weigh() {
    let model = model_of(vec![opened(submodel("detail0", None, [0.0; 3], &[([-1.0; 3], [1.0; 3])]))], 100.0);

    assert_eq!(model.recalc_moi(SOLID_SKIPPING).err(), Some(MassPropertiesError::NothingToWeigh));
}

#[test]
fn a_two_sided_sheet_is_closed_and_weighs_nothing() {
    let hull = ([-2.0, -1.0, -4.0], [2.0, 1.0, 4.0]);
    let alone = model_of(vec![submodel("detail0", None, [0.0; 3], &[hull])], 75.0);

    // a fin well clear of the origin, where its two sides don't cancel to the last bit
    let fin = vec![
        Vec3d::new(0.3, 1.1, 0.7),
        Vec3d::new(0.3, 3.7, 0.9),
        Vec3d::new(0.3, 3.3, 2.9),
        Vec3d::new(0.3, 1.3, 2.3),
    ];
    let fin_faces = vec![vec![0, 1, 2, 3], vec![3, 2, 1, 0]];

    let mut part_of_hull = model_of(vec![submodel("detail0", None, [0.0; 3], &[hull])], 75.0);
    let bsp_data = &mut part_of_hull.submodels.0[0].bsp_data;
    let mut faces: Vec<Vec<u32>> = FACES.iter().map(|face| face.to_vec()).collect();
    faces.extend(fin_faces.iter().map(|face| face.iter().map(|i| i + 8).collect()));
    *bsp_data = mesh_of(bsp_data.verts.iter().copied().chain(fin.iter().copied()).collect(), faces);
    assert_close(moi(&part_of_hull, SOLID), moi(&alone, SOLID));

    for offset in [[0.0; 3], [1.7, -0.3, 2.1], [-31.3, 17.9, 0.7]] {
        let mut on_its_own = model_of(vec![submodel("detail0", None, [0.0; 3], &[hull]), submodel("fin", Some(0), offset, &[])], 75.0);
        on_its_own.submodels.0[1].bsp_data = mesh_of(fin.clone(), fin_faces.clone());
        assert_close(moi(&on_its_own, SOLID), moi(&alone, SOLID));
    }
}

#[test]
fn a_one_sided_sheet_cant_be_solid() {
    let mut model = model_of(vec![submodel("detail0", None, [0.0; 3], &[])], 75.0);
    let sheet = vec![
        Vec3d::new(0.0, 0.0, 0.0),
        Vec3d::new(4.0, 0.0, 0.0),
        Vec3d::new(4.0, 2.0, 0.0),
        Vec3d::new(0.0, 2.0, 0.0),
    ];
    model.submodels.0[0].bsp_data = mesh_of(sheet, vec![vec![0, 1, 2, 3]]);

    assert_eq!(model.recalc_moi(SOLID).err(), Some(bad_mesh(&[0], &[])));
}

#[test]
fn a_cavity_takes_away_from_the_solid_around_it() {
    // a cube of side 4 around a cube of side 2 that faces inwards
    let model = model_of(
        vec![submodel(
            "detail0",
            None,
            [0.0; 3],
            &[([-2.0; 3], [2.0; 3]), ([1.0, -1.0, -1.0], [-1.0, 1.0, 1.0])],
        )],
        100.0,
    );

    // at unit density a cube's moment is L^5 / 6, and its volume L^3
    let expected = (64.0 - 8.0) / (100.0 * (1024.0 - 32.0) / 6.0);
    assert_close(moi(&model, SOLID), diagonal(expected, expected, expected));
}

#[test]
fn verts_are_matched_by_where_they_are() {
    let a_box = ([0.0, 0.0, 0.0], [2.0, 3.0, 1.0]);
    let welded = model_of(vec![submodel("detail0", None, [0.0; 3], &[a_box])], 100.0);
    let mut unwelded = model_of(vec![submodel("detail0", None, [0.0; 3], &[])], 100.0);
    unwelded.submodels.0[0].bsp_data = unwelded_mesh(&a_box);
    assert_close(moi(&unwelded, SOLID), moi(&welded, SOLID));

    // every other face has its zeroes negative
    let bsp_data = &mut unwelded.submodels.0[0].bsp_data;
    for vert in bsp_data.verts.chunks_mut(4).step_by(2).flatten() {
        for val in [&mut vert.x, &mut vert.y, &mut vert.z] {
            if *val == 0.0 {
                *val = -0.0;
            }
        }
    }
    assert!(bsp_data.verts.iter().any(|vert| vert.x.to_bits() == (-0.0f32).to_bits()));
    assert!(bsp_data.verts.iter().any(|vert| vert.x.to_bits() == 0.0f32.to_bits()));
    assert_close(moi(&unwelded, SOLID), moi(&welded, SOLID));
}

#[test]
fn verts_that_only_nearly_meet_leave_the_mesh_open() {
    let mut model = model_of(vec![submodel("detail0", None, [0.0; 3], &[])], 100.0);
    model.submodels.0[0].bsp_data = unwelded_mesh(&([0.0, 0.0, 0.0], [2.0, 3.0, 1.0]));
    model.submodels.0[0].bsp_data.verts[0].x += 1e-5;

    assert_eq!(model.recalc_moi(SOLID).err(), Some(bad_mesh(&[0], &[])));
}

// ---------------------------------------------------------------- capping the holes

#[test]
fn holes_are_capped_unless_asked_otherwise() {
    assert_eq!(MassModel::default(), SOLID_CAPPING);
}

#[test]
fn a_flat_hole_is_capped_with_what_was_missing() {
    let hull = ([-2.0, -1.0, -4.0], [2.0, 1.0, 4.0]);
    let turret = ([-1.0, 0.0, -1.0], [1.0, 1.0, 1.5]);
    let closed = model_of(
        vec![
            submodel("detail0", None, [0.0; 3], &[hull]),
            submodel("turret01", Some(0), [3.0, 1.0, -2.0], &[turret]),
        ],
        75.0,
    );

    for (hull_missing, turret_missing) in [(vec![3], vec![]), (vec![], vec![2]), (vec![0, 1], vec![5]), (vec![4], vec![2, 3])] {
        let mut model = model_of(
            vec![
                submodel("detail0", None, [0.0; 3], &[]),
                submodel("turret01", Some(0), [3.0, 1.0, -2.0], &[]),
            ],
            75.0,
        );
        model.submodels.0[0].bsp_data = box_without(&hull, &hull_missing);
        model.submodels.0[1].bsp_data = box_without(&turret, &turret_missing);

        assert!(matches!(model.recalc_moi(SOLID), Err(MassPropertiesError::BadMesh { .. })));
        assert_close(moi(&model, SOLID_CAPPING), moi(&closed, SOLID));
        assert_vec_close(center_of_mass(&model, SOLID_CAPPING), center_of_mass(&closed, SOLID));
        assert_eq!(model.recalc_moi(SOLID_CAPPING_AND_SKIPPING).unwrap().1, []);
    }
}

#[test]
fn a_hole_that_bends_is_left_open() {
    // two faces that share an edge leave a hole that runs around a corner
    let mut model = model_of(vec![submodel("detail0", None, [0.0; 3], &[])], 75.0);
    model.submodels.0[0].bsp_data = box_without(&([-2.0, -1.0, -4.0], [2.0, 1.0, 4.0]), &[1, 3]);

    assert_eq!(model.recalc_moi(SOLID_CAPPING).err(), Some(bad_mesh(&[0], &[])));
    assert_eq!(model.recalc_moi(SOLID_CAPPING_AND_SKIPPING).err(), Some(MassPropertiesError::NothingToWeigh));
}

#[test]
fn a_hole_is_flat_to_within_a_little() {
    for (lift, flat) in [(0.0, true), (0.01, true), (1.0, false)] {
        let mut model = model_of(vec![submodel("detail0", None, [0.0; 3], &[])], 75.0);
        model.submodels.0[0].bsp_data = box_without(&([-2.0, -1.0, -4.0], [2.0, 1.0, 4.0]), &[3]);
        model.submodels.0[0].bsp_data.verts[7].y += lift;

        assert_eq!(model.recalc_moi(SOLID_CAPPING).is_ok(), flat, "lifted by {}", lift);
    }
}

#[test]
fn every_hole_has_to_be_flat() {
    // a flat hole in the one box, and a bent one in the other
    let mut model = model_of(vec![submodel("detail0", None, [0.0; 3], &[])], 75.0);
    let mut bsp_data = box_without(&([-2.0, -1.0, -4.0], [2.0, 1.0, 4.0]), &[2]);
    let bent = box_without(&([5.0, -1.0, -4.0], [7.0, 1.0, 4.0]), &[1, 3]);
    let polygons = bent.collision_tree.into_leaves().map(|(_, mut poly)| {
        poly.verts.iter_mut().for_each(|vert| vert.vertex_id.0 += 8);
        poly
    });
    bsp_data.verts.extend(bent.verts);
    let polygons: Vec<_> = std::mem::take(&mut bsp_data.collision_tree)
        .into_leaves()
        .map(|(_, poly)| poly)
        .chain(polygons)
        .collect();
    bsp_data.collision_tree = BspData::recalculate(&bsp_data.verts, polygons.into_iter());
    model.submodels.0[0].bsp_data = bsp_data;

    assert_eq!(model.recalc_moi(SOLID_CAPPING).err(), Some(bad_mesh(&[0], &[])));
}

#[test]
fn a_one_sided_sheet_is_capped_with_its_other_side() {
    let hull = ([-2.0, -1.0, -4.0], [2.0, 1.0, 4.0]);
    let alone = model_of(vec![submodel("detail0", None, [0.0; 3], &[hull])], 75.0);
    let mut with_sheet = model_of(
        vec![
            submodel("detail0", None, [0.0; 3], &[hull]),
            submodel("fin", Some(0), [1.7, 2.0, 2.1], &[]),
        ],
        75.0,
    );
    let sheet = vec![
        Vec3d::new(0.3, 1.1, 0.7),
        Vec3d::new(0.3, 3.7, 0.9),
        Vec3d::new(0.3, 3.3, 2.9),
        Vec3d::new(0.3, 1.3, 2.3),
    ];
    with_sheet.submodels.0[1].bsp_data = mesh_of(sheet, vec![vec![0, 1, 2, 3]]);

    assert_eq!(with_sheet.recalc_moi(SOLID).err(), Some(bad_mesh(&[1], &[])));
    assert_close(moi(&with_sheet, SOLID_CAPPING), moi(&alone, SOLID));
}

#[test]
fn an_edge_met_by_two_shorter_ones_is_capped_with_nothing() {
    // the top is split in two down the middle, and the sides it meets aren't
    let a_box = ([-2.0, -1.0, -4.0], [2.0, 1.0, 4.0]);
    let closed = model_of(vec![submodel("detail0", None, [0.0; 3], &[a_box])], 75.0);
    let mut split = model_of(vec![submodel("detail0", None, [0.0; 3], &[])], 75.0);
    let mut verts: Vec<_> = corners(&a_box).collect();
    verts.extend([Vec3d::new(0.0, 1.0, 4.0), Vec3d::new(0.0, 1.0, -4.0)]);
    let mut faces: Vec<_> = FACES.iter().map(|face| face.to_vec()).collect();
    faces[3] = vec![6, 8, 9, 2];
    faces.push(vec![8, 7, 3, 9]);
    split.submodels.0[0].bsp_data = mesh_of(verts, faces);

    assert_eq!(split.recalc_moi(SOLID).err(), Some(bad_mesh(&[0], &[])));
    assert_close(moi(&split, SOLID_CAPPING), moi(&closed, SOLID));
}

#[test]
fn a_capped_mesh_can_still_face_inwards() {
    let mut model = model_of(vec![submodel("detail0", None, [0.0; 3], &[])], 75.0);
    model.submodels.0[0].bsp_data = box_without(&([2.0, -1.0, -4.0], [-2.0, 1.0, 4.0]), &[3]);

    assert_eq!(model.recalc_moi(SOLID_CAPPING).err(), Some(bad_mesh(&[], &[0])));
}

// ---------------------------------------------------------------- center of mass

#[test]
fn a_box_is_centered_on_its_middle() {
    let model = model_of(
        vec![
            submodel("detail0", None, [0.0; 3], &[]),
            submodel("pod", Some(0), [3.0, 1.0, -2.0], &[([-1.0; 3], [1.0; 3])]),
        ],
        100.0,
    );

    for mass_model in [SOLID, SHELL] {
        assert_vec_close(center_of_mass(&model, mass_model), Vec3d::new(3.0, 1.0, -2.0));
    }
}

#[test]
fn a_solid_is_centered_by_volume_and_a_shell_by_area() {
    // a cube of side 2 at x = 0, and one of side 4 at x = 10
    let model = model_of(
        vec![submodel(
            "detail0",
            None,
            [0.0; 3],
            &[([-1.0; 3], [1.0; 3]), ([8.0, -2.0, -2.0], [12.0, 2.0, 2.0])],
        )],
        100.0,
    );

    assert_vec_close(center_of_mass(&model, SOLID), Vec3d::new(10.0 * 64.0 / 72.0, 0.0, 0.0));
    assert_vec_close(center_of_mass(&model, SHELL), Vec3d::new(10.0 * 96.0 / 120.0, 0.0, 0.0));
}

// ---------------------------------------------------------------- transforming the model

#[test]
fn a_transform_carries_the_tensor_with_it() {
    let transforms = [
        ("rotation", glm::rotation(0.7, &glm::vec3(1.0, -2.0, 0.5).normalize())),
        ("uniform scale", glm::scaling(&glm::vec3(2.5, 2.5, 2.5))),
        ("mirror", glm::scaling(&glm::vec3(-1.0, 1.0, 1.0))),
        ("mirror and turn", glm::rotation(0.4, &glm::vec3(0.0, 1.0, 0.0)) * glm::scaling(&glm::vec3(1.0, 1.0, -1.0))),
    ];

    for mass_model in [SOLID, SHELL] {
        for (name, matrix) in &transforms {
            let mut model = lopsided_model(mass_model);
            let before = model.header.moment_of_inertia;
            model.apply_transform(matrix);

            println!("{} {:?}", name, mass_model);
            assert_close(model.header.moment_of_inertia, moi(&model, mass_model));
            assert!(glm::Mat3x3::from(model.header.moment_of_inertia) != glm::Mat3x3::from(before));
        }
    }
}

#[test]
fn a_stretch_is_carried_exactly_by_a_solid() {
    let mut model = lopsided_model(SOLID);
    model.apply_transform(&glm::scaling(&glm::vec3(3.0, 0.5, 2.0)));

    assert_close(model.header.moment_of_inertia, moi(&model, SOLID));
}

#[test]
fn a_stretch_moves_a_shells_mass_without_spreading_it_afresh() {
    let mut model = model_of(vec![submodel("detail0", None, [0.0; 3], &[([-1.0; 3], [1.0; 3])])], 100.0);
    model.header.moment_of_inertia = moi(&model, SHELL);
    model.apply_transform(&glm::scaling(&glm::vec3(2.0, 1.0, 1.0)));

    // the cube's second moment is 5/36 m L^2 on each axis, and the stretch quadruples it along x
    let second_moment = 5.0 * 100.0 * 2.0 * 2.0 / 36.0;
    let expected = diagonal(1.0 / (2.0 * second_moment), 1.0 / (5.0 * second_moment), 1.0 / (5.0 * second_moment));
    assert_close(model.header.moment_of_inertia, expected);
}

#[test]
fn a_mirrored_mesh_still_faces_outwards() {
    let mut model = lopsided_model(SOLID);
    let mirror = glm::scaling(&glm::vec3(1.0, -1.0, 1.0));
    model.apply_submodel_transform_mesh(SubmodelId(1), &mirror);
    assert!(model.recalc_moi(SOLID).is_ok());

    model.apply_submodel_transform(SubmodelId(0), &mirror, true);
    assert!(model.recalc_moi(SOLID).is_ok());

    for smodel in &model.submodels {
        let middle = Vec3d::average(smodel.bsp_data.verts.iter().copied());
        for (_, poly) in smodel.bsp_data.collision_tree.leaves() {
            let center = Vec3d::average(poly.verts.iter().map(|vert| smodel.bsp_data.verts[vert.vertex_id.0 as usize]));
            assert!(poly.normal.dot(&(center - middle)) > 0.0);
        }
    }
}

#[test]
fn a_transform_and_its_reverse_restore_the_tensor() {
    let mut model = lopsided_model(SOLID);
    let before = model.header.moment_of_inertia;
    model.apply_transform(&glm::scaling(&glm::vec3(3.0, 0.5, 2.0)));
    model.apply_transform(&glm::scaling(&glm::vec3(1.0 / 3.0, 2.0, 0.5)));

    assert_close(model.header.moment_of_inertia, before);
}

#[test]
fn a_translation_moves_the_center_of_mass_and_leaves_the_tensor() {
    let mut model = lopsided_model(SOLID);
    model.header.center_of_mass = center_of_mass(&model, SOLID);
    let before = model.header.moment_of_inertia;
    model.apply_transform(&glm::translation(&glm::vec3(4.0, -1.0, 2.0)));

    assert_close(model.header.moment_of_inertia, before);
    assert_vec_close(model.header.center_of_mass, center_of_mass(&model, SOLID));
}

#[test]
fn a_tensor_that_cant_be_inverted_is_left_as_it_was() {
    let mut model = lopsided_model(SOLID);
    model.header.moment_of_inertia = Mat3d::default();
    model.apply_transform(&glm::scaling(&glm::vec3(2.0, 2.0, 2.0)));

    assert!(glm::Mat3x3::from(model.header.moment_of_inertia) == glm::Mat3x3::zeros());
}

// ---------------------------------------------------------------- surface centroid

#[test]
fn a_polygon_counts_in_full_whatever_its_vert_count() {
    let mut smodel = submodel("detail0", None, [0.0; 3], &[]);
    let sheet = vec![
        Vec3d::new(0.0, 0.0, 0.0),
        Vec3d::new(4.0, 0.0, 0.0),
        Vec3d::new(4.0, 2.0, 0.0),
        Vec3d::new(0.0, 2.0, 0.0),
    ];
    smodel.bsp_data = mesh_of(sheet, vec![vec![0, 1, 2, 3]]);

    let (area, center) = smodel.surface_area_average_pos();
    assert!((area - 8.0).abs() <= 1e-4);
    assert_vec_close(center, Vec3d::new(2.0, 1.0, 0.0));
}

#[test]
fn each_submodel_is_counted_once() {
    let model = model_of(
        vec![
            submodel("detail0", None, [0.0; 3], &[([-1.0; 3], [1.0; 3])]),
            submodel("pod", Some(0), [4.0, 0.0, 0.0], &[([-1.0; 3], [1.0; 3])]),
        ],
        100.0,
    );

    let (area, center) = model.surface_area_average_pos();
    assert!((area - 48.0).abs() <= 1e-3);
    assert_vec_close(center, Vec3d::new(2.0, 0.0, 0.0));
}

#[test]
fn a_submodel_with_no_geometry_doesnt_spoil_the_average() {
    let model = model_of(
        vec![
            submodel("detail0", None, [0.0; 3], &[]),
            submodel("pod", Some(0), [3.0, 1.0, -2.0], &[([-1.0; 3], [1.0; 3])]),
        ],
        100.0,
    );

    assert_vec_close(model.surface_area_average_pos().1, Vec3d::new(3.0, 1.0, -2.0));

    let empty = model_of(vec![submodel("detail0", None, [0.0; 3], &[])], 100.0);
    let (area, center) = empty.surface_area_average_pos();
    assert_eq!(area, 0.0);
    assert_vec_close(center, Vec3d::ZERO);
}

#[test]
fn the_recursive_walk_visits_each_submodel_once() {
    let model = model_of(
        vec![
            submodel("detail0", None, [0.0; 3], &[]),
            submodel("turret01", Some(0), [0.0; 3], &[]),
            submodel("turret01-arm", Some(1), [0.0; 3], &[]),
            submodel("pod", Some(0), [0.0; 3], &[]),
        ],
        100.0,
    );

    let mut visited = vec![];
    model.do_for_recursive_smodel_children(SubmodelId(0), &mut |smodel| visited.push(smodel.id.0));
    visited.sort();
    assert_eq!(visited, [0, 1, 2, 3]);
}

// ---------------------------------------------------------------- the warning

#[test]
fn a_tensor_fso_would_complain_about_is_warned_of() {
    let mut model = lopsided_model(SOLID);
    let valid = model.header.moment_of_inertia;

    model.recheck_warnings(Set::All);
    assert!(!model.warnings.contains(&Warning::InvalidMomentOfInertia));

    model.header.moment_of_inertia = Mat3d::default();
    model.recheck_warnings(Set::All);
    assert!(model.warnings.contains(&Warning::InvalidMomentOfInertia));

    model.header.moment_of_inertia = valid;
    model.recheck_warnings(Set::One(Warning::InvalidMomentOfInertia));
    assert!(!model.warnings.contains(&Warning::InvalidMomentOfInertia));

    for bad in [f32::NAN, f32::INFINITY] {
        model.header.moment_of_inertia = valid;
        model.header.moment_of_inertia.uvec.z = bad;
        model.recheck_warnings(Set::One(Warning::InvalidMomentOfInertia));
        assert!(model.warnings.contains(&Warning::InvalidMomentOfInertia), "{}", bad);
    }
}

#[test]
fn a_blank_model_isnt_warned_about() {
    let mut model = Model::default();
    model.recheck_warnings(Set::All);

    assert!(!model.warnings.contains(&Warning::InvalidMomentOfInertia));
}
