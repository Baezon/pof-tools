use std::{
    borrow::Cow,
    f32::consts::PI,
    io::{self, Write},
    mem::size_of,
};

use byteorder::{WriteBytesExt, LE};
use dae_parser::Document;
use glm::{Mat4x4, Vec3};
use gltf::buffer::Target;
use gltf_json as json;
use json::accessor::ComponentType;
use json::validation::Checked::Valid;
use json::Index;
extern crate nalgebra_glm as glm;

use crate::{
    BoundingBox, BspData, BspNode, Dock, EyePoint, GlowPointBank, Insignia, Model, ObjVec, ObjectId, Path, ShieldData, ShieldNode, SpecialPoint,
    SubObject, ThrusterBank, Turret, Vec3d, Version, WeaponHardpoint,
};

pub(crate) trait Serialize {
    fn write_to(&self, w: &mut impl Write) -> io::Result<()>;
}

impl Serialize for () {
    fn write_to(&self, _: &mut impl Write) -> io::Result<()> {
        Ok(())
    }
}

impl<T: Serialize> Serialize for Option<T> {
    fn write_to(&self, w: &mut impl Write) -> io::Result<()> {
        if let Some(x) = self {
            x.write_to(w)?
        }
        Ok(())
    }
}

impl Serialize for String {
    fn write_to(&self, w: &mut impl Write) -> io::Result<()> {
        (**self).write_to(w)
    }
}
impl Serialize for str {
    fn write_to(&self, w: &mut impl Write) -> io::Result<()> {
        let padding_length = (3_usize.wrapping_sub(self.len()) % 4) + 1;
        w.write_u32::<LE>((self.len() + padding_length) as u32)?;

        w.write_all(self.as_bytes())?;

        w.write_all(&[0; 4][..padding_length])
    }
}

impl<A: Serialize, B: Serialize, C: Serialize> Serialize for (A, B, C) {
    fn write_to(&self, w: &mut impl Write) -> io::Result<()> {
        self.0.write_to(w)?;
        self.1.write_to(w)?;
        self.2.write_to(w)
    }
}
impl<A: Serialize, B: Serialize> Serialize for (A, B) {
    fn write_to(&self, w: &mut impl Write) -> io::Result<()> {
        self.0.write_to(w)?;
        self.1.write_to(w)
    }
}
impl<T: Serialize> Serialize for Vec<T> {
    fn write_to(&self, w: &mut impl Write) -> io::Result<()> {
        (**self).write_to(w)
    }
}
impl<T: Serialize> Serialize for [T] {
    fn write_to(&self, w: &mut impl Write) -> io::Result<()> {
        w.write_u32::<LE>((self.len()) as u32)?;

        for item in self {
            item.write_to(w)?;
        }
        Ok(())
    }
}

impl Serialize for u32 {
    fn write_to(&self, w: &mut impl Write) -> io::Result<()> {
        w.write_u32::<LE>(*self)
    }
}
impl Serialize for u16 {
    fn write_to(&self, w: &mut impl Write) -> io::Result<()> {
        w.write_u16::<LE>(*self)
    }
}
impl Serialize for u8 {
    fn write_to(&self, w: &mut impl Write) -> io::Result<()> {
        w.write_u8(*self)
    }
}
impl Serialize for f32 {
    fn write_to(&self, w: &mut impl Write) -> io::Result<()> {
        w.write_f32::<LE>(*self)
    }
}
impl Serialize for i32 {
    fn write_to(&self, w: &mut impl Write) -> io::Result<()> {
        w.write_i32::<LE>(*self)
    }
}
impl Serialize for crate::NormalVec3 {
    fn write_to(&self, w: &mut impl Write) -> io::Result<()> {
        self.0.write_to(w)
    }
}

fn align_buf(buf: &mut Vec<u8>) -> io::Result<()> {
    let padding_length = buf.len().wrapping_neg() % 4;
    buf.write_all(&[0; 4][..padding_length])
}

pub(crate) fn write_shield_node(buf: &mut Vec<u8>, shield_node: &ShieldNode, chunk_type_is_u8: bool) -> io::Result<()> {
    macro_rules! write_chunk_type {
        ($n:expr) => {
            if chunk_type_is_u8 {
                buf.write_u8($n as u8)
            } else {
                buf.write_u32::<LE>($n)
            }
        };
    }
    match shield_node {
        ShieldNode::Split { bbox, front, back } => {
            let base = buf.len();
            write_chunk_type!(ShieldNode::SPLIT)?;
            let chunk_size_pointer = Fixup::new(buf, base)?;

            bbox.write_to(buf)?;
            let front_offset = Fixup::new(buf, base)?;
            let back_offset = Fixup::new(buf, base)?;

            front_offset.finish(buf);
            write_shield_node(buf, front, chunk_type_is_u8)?;

            back_offset.finish(buf);
            write_shield_node(buf, back, chunk_type_is_u8)?;

            chunk_size_pointer.finish(buf);
        }
        ShieldNode::Leaf { bbox, poly_list } => {
            let base = buf.len();
            write_chunk_type!(ShieldNode::LEAF)?;
            let chunk_size_pointer = Fixup::new(buf, base)?;

            bbox.write_to(buf)?;
            poly_list.write_to(buf)?;
            chunk_size_pointer.finish(buf);
        }
    }
    Ok(())
}

#[must_use]
struct Fixup {
    location: usize,
    base: usize,
}
impl Fixup {
    fn new(buf: &mut Vec<u8>, base: usize) -> io::Result<Self> {
        let n = buf.len();
        buf.write_u32::<LE>(0)?;
        Ok(Self { location: n, base })
    }
    fn finish(self, buf: &mut Vec<u8>) -> u32 {
        let size = (buf.len() - self.base) as u32;
        buf[self.location..][..4].copy_from_slice(&u32::to_le_bytes(size));
        size
    }
}

pub(crate) fn write_bsp_data(buf: &mut Vec<u8>, version: Version, bsp_data: &BspData) -> io::Result<()> {
    const MAX_NORMS_PER_VERT: u8 = 0xCC; //u8::MAX;

    fn write_bsp_node(buf: &mut Vec<u8>, verts: &[Vec3d], version: Version, bsp_node: &BspNode) -> io::Result<()> {
        match bsp_node {
            BspNode::Split { bbox, front, back } => {
                let base = buf.len();
                if version >= Version::V23_00 {
                    buf.write_u32::<LE>(BspData::SORTNORM2)?;
                } else {
                    buf.write_u32::<LE>(BspData::SORTNORM)?;
                }
                let chunk_size_pointer = Fixup::new(buf, base)?;

                if version < Version::V23_00 {
                    Vec3d::ZERO.write_to(buf)?; // plane_normal: unused
                    Vec3d::ZERO.write_to(buf)?; // plane_point: unused
                    buf.write_u32::<LE>(0)?; // reserved: unused
                }

                let front_offset = Fixup::new(buf, base)?;
                let back_offset = Fixup::new(buf, base)?;

                if version < Version::V23_00 {
                    buf.write_u32::<LE>(0)?; // prelist_offset: unused
                    buf.write_u32::<LE>(0)?; // postlist_offset: unused
                    buf.write_u32::<LE>(0)?; // online_offset: unused
                }
                if version >= Version::V20_00 {
                    bbox.write_to(buf)?;
                }

                if !matches!(**front, BspNode::Empty) {
                    front_offset.finish(buf);
                    write_bsp_node(buf, verts, version, front)?;
                } // otherwise front_offset = 0

                if !matches!(**back, BspNode::Empty) {
                    back_offset.finish(buf);
                    write_bsp_node(buf, verts, version, back)?;
                } // otherwise back_offset = 0

                chunk_size_pointer.finish(buf);

                if version < Version::V23_00 {
                    buf.write_u32::<LE>(BspData::ENDOFBRANCH)?;
                    buf.write_u32::<LE>(0)?;
                }
            }
            BspNode::Leaf { bbox, poly } => {
                let base = buf.len();
                if version >= Version::V23_00 {
                    buf.write_u32::<LE>(BspData::TMAPPOLY2)?;
                    let chunk_size_pointer = Fixup::new(buf, base)?;

                    bbox.write_to(buf)?;

                    poly.normal.write_to(buf)?;
                    poly.texture.write_to(buf)?;
                    (poly.verts.len() as u32).write_to(buf)?;

                    for vert in &poly.verts {
                        vert.vertex_id.0.write_to(buf)?;
                        vert.normal_id.0.write_to(buf)?;
                        vert.uv.write_to(buf)?;
                    }

                    chunk_size_pointer.finish(buf);
                } else {
                    buf.write_u32::<LE>(BspData::BOUNDBOX)?;
                    let chunk_size_pointer = Fixup::new(buf, base)?;

                    bbox.write_to(buf)?;
                    chunk_size_pointer.finish(buf);

                    let base = buf.len();
                    buf.write_u32::<LE>(BspData::TMAPPOLY)?;
                    let chunk_size_pointer = Fixup::new(buf, base)?;

                    poly.normal.write_to(buf)?;
                    Vec3d::ZERO.write_to(buf)?; // center: unused
                    0f32.write_to(buf)?; // radius: unused
                    (poly.verts.len() as u32).write_to(buf)?;
                    poly.texture.write_to(buf)?;

                    for vert in &poly.verts {
                        u16::try_from(vert.vertex_id.0).unwrap().write_to(buf)?;
                        u16::try_from(vert.normal_id.0).unwrap().write_to(buf)?;
                        vert.uv.write_to(buf)?;
                    }

                    chunk_size_pointer.finish(buf);

                    buf.write_u32::<LE>(BspData::ENDOFBRANCH)?;
                    buf.write_u32::<LE>(0)?;
                }
            }
            BspNode::Empty => {
                buf.write_u32::<LE>(BspData::ENDOFBRANCH)?;
                buf.write_u32::<LE>(0)?;
            }
        }
        Ok(())
    }

    let base = buf.len();
    buf.write_u32::<LE>(BspData::DEFFPOINTS)?;

    let chunk_size_pointer = Fixup::new(buf, base)?;

    (bsp_data.verts.len() as u32).write_to(buf)?;
    (bsp_data.norms.len() as u32).write_to(buf)?;
    let vertex_data_offset = Fixup::new(buf, base)?;

    let mut num_norms = bsp_data.norms.len();
    for _ in 0..bsp_data.verts.len() {
        let old_num_norms = num_norms;
        num_norms = num_norms.saturating_sub(MAX_NORMS_PER_VERT.into());

        buf.push((old_num_norms - num_norms) as u8);
    }
    assert!(num_norms == 0);

    align_buf(buf)?;

    vertex_data_offset.finish(buf);

    let mut norm_iter = bsp_data.norms.iter();
    let mut num_norms = bsp_data.norms.len();
    for &vert in &bsp_data.verts {
        vert.write_to(buf)?;

        let old_num_norms = num_norms;
        num_norms = num_norms.saturating_sub(MAX_NORMS_PER_VERT.into());

        for _ in num_norms..old_num_norms {
            norm_iter.next().unwrap().write_to(buf)?;
        }
    }

    chunk_size_pointer.finish(buf);

    write_bsp_node(buf, &bsp_data.verts, version, &bsp_data.collision_tree)?;

    Ok(())
}

fn write_chunk_raw(w: &mut impl Write, chunk_name: &[u8; 4], f: impl FnOnce(&mut Vec<u8>) -> io::Result<()>) -> io::Result<()> {
    w.write_all(chunk_name)?;
    //println!("writing chunk {}", std::str::from_utf8(chunk_name).unwrap());

    let mut buf = vec![];

    f(&mut buf)?;

    w.write_u32::<LE>(buf.len() as u32)?;
    //println!("writing chunk length {}", buf.len());
    w.write_all(&buf)
}

fn write_chunk<T: Serialize>(w: &mut impl Write, chunk_name: &[u8; 4], data: Option<&T>) -> io::Result<()> {
    if let Some(data) = data {
        write_chunk_raw(w, chunk_name, |w| data.write_to(w))?
    }
    Ok(())
}

fn write_chunk_vec<T: Serialize>(w: &mut impl Write, chunk_name: &[u8; 4], data: &[T]) -> io::Result<()> {
    if !data.is_empty() {
        write_chunk_raw(w, chunk_name, |w| data.write_to(w))?
    }
    Ok(())
}

fn write_subobjects(w: &mut impl Write, chunk_name: &[u8; 4], objects: &[SubObject]) -> io::Result<()> {
    fn write_subobject(w: &mut impl Write, chunk_name: [u8; 4], objects: &[SubObject], written: &mut [bool], id: ObjectId) -> io::Result<()> {
        if !written[id.0 as usize] {
            let obj = &objects[id.0 as usize];
            // ensure parents are written before children
            if let Some(parent) = obj.parent {
                write_subobject(w, chunk_name, objects, written, parent)?;
            }
            write_chunk(w, &chunk_name, Some(obj))?;
            written[id.0 as usize] = true;
        }
        Ok(())
    }

    let mut written = vec![false; objects.len()];
    for i in 0..objects.len() as u32 {
        write_subobject(w, *chunk_name, objects, &mut written, ObjectId(i))?;
    }
    Ok(())
}

impl Model {
    pub fn write(&self, w: &mut impl Write) -> io::Result<()> {
        // set the version to be using be all the serializers
        crate::VERSION.with(|f| {
            f.set(self.version);
        });
        w.write_all(b"PSPO")?;

        w.write_i32::<LE>(self.version.into())?;

        write_chunk_raw(w, if self.version >= Version::V21_16 { b"HDR2" } else { b"OHDR" }, |w| {
            if self.version >= Version::V21_16 {
                self.header.max_radius.write_to(w)?;
                self.header.obj_flags.write_to(w)?;
                self.header.num_subobjects.write_to(w)?;
            } else {
                self.header.num_subobjects.write_to(w)?;
                self.header.max_radius.write_to(w)?;
                self.header.obj_flags.write_to(w)?;
            }
            self.header.bbox.write_to(w)?;
            self.header.detail_levels.write_to(w)?;
            self.sub_objects
                .iter()
                .filter(|obj| obj.is_debris_model)
                .map(|obj| obj.obj_id)
                .collect::<Vec<_>>()
                .write_to(w)?;
            if self.version >= Version::V20_09 {
                self.header.mass.write_to(w)?;
                self.header.center_of_mass.write_to(w)?;
                self.header.moment_of_inertia.write_to(w)?;
            } else if self.version >= Version::V19_03 {
                let vol_mass = (self.header.mass / 4.65).powf(1.5);
                vol_mass.write_to(w)?;
                self.header.center_of_mass.write_to(w)?;
                let mut moi = self.header.moment_of_inertia;
                moi *= self.header.mass / vol_mass;
                moi.write_to(w)?;
            }
            if self.version >= Version::V20_14 {
                self.header.cross_sections.write_to(w)?;
            }
            if self.version >= Version::V20_07 {
                self.header.bsp_lights.write_to(w)?;
            }
            Ok(())
        })?;
        write_subobjects(w, if self.version >= Version::V21_16 { b"OBJ2" } else { b"SOBJ" }, &self.sub_objects)?;
        write_chunk_vec(w, b"TXTR", &self.textures)?;
        write_chunk_vec(w, b"PATH", &self.paths)?;
        write_chunk_vec(w, b"SPCL", &self.special_points)?;
        write_chunk_vec(w, b"EYE ", &self.eye_points)?;
        if !self.primary_weps.is_empty() {
            write_chunk(w, b"GPNT", Some(&self.primary_weps))?;
        }
        if !self.secondary_weps.is_empty() {
            write_chunk(w, b"MPNT", Some(&self.secondary_weps))?;
        }
        if !self.turrets.is_empty() {
            write_chunk(w, b"TGUN", Some(&self.turrets))?;
        }
        write_chunk_vec(w, b"FUEL", &self.thruster_banks)?;
        write_chunk_vec(w, b"GLOW", &self.glow_banks)?;
        if !self.comments.is_empty() {
            write_chunk_raw(w, b"PINF", |w| {
                let padding_length = (3_usize.wrapping_sub(self.comments.len()) % 4) + 1;
                w.write_all(self.comments.as_bytes())?;

                w.write_all(&[0; 4][..padding_length])
            })?;
        }
        write_chunk_vec(w, b"DOCK", &self.docking_bays)?;
        write_chunk_vec(w, b"INSG", &self.insignias)?;
        if let Some(shield_data) = &self.shield_data {
            write_chunk_raw(w, b"SHLD", |w| {
                shield_data.verts.write_to(w)?;
                shield_data.polygons.write_to(w)
            })?;

            if self.version >= Version::V21_18 {
                write_chunk(w, if self.version >= Version::V22_00 { b"SLC2" } else { b"SLDC" }, shield_data.collision_tree.as_ref())?;
            }
        }
        if self.visual_center != Vec3d::default() {
            write_chunk(w, b"ACEN", Some(&self.visual_center))?;
        }

        Ok(())
    }
}

// ==============================================================================
// DAE / glTF Writing
// ==============================================================================

use dae_parser::{Node as DaeNode, *};

trait NodeBuilder {
    type Ctx;
    type Node;
    fn children(&mut self) -> &mut Vec<Self::Node>;
    fn translate(&mut self, val: [f32; 3]);
    fn rotate(&mut self, axis_angle: (Vec3d, f32));
    fn scale(&mut self, val: [f32; 3]);
    fn matrix_transform(&mut self, mat: Mat4x4);
    fn build(self, ctx: &mut Self::Ctx) -> Self::Node;
}
trait Node {
    type Ctx;
    type Builder: NodeBuilder<Node = Self, Ctx = Self::Ctx>;
    fn from_name(id: String, name: String) -> Self::Builder;
    fn from_id(id: String) -> Self::Builder {
        Self::from_name(id.clone(), id)
    }
}
impl NodeBuilder for DaeNode {
    type Ctx = ();
    type Node = Self;
    fn children(&mut self) -> &mut Vec<DaeNode> {
        &mut self.children
    }
    fn translate(&mut self, val: [f32; 3]) {
        self.push_transform(Translate::new(val))
    }
    fn rotate(&mut self, (axis, angle): (Vec3d, f32)) {
        self.push_transform(Rotate::new(axis.into(), angle * (180.0 / PI)))
    }
    fn scale(&mut self, val: [f32; 3]) {
        self.push_transform(Scale::new(val))
    }
    fn matrix_transform(&mut self, mat: Mat4x4) {
        self.push_transform(mat)
    }
    fn build(self, _: &mut ()) -> DaeNode {
        self
    }
}
impl Node for DaeNode {
    type Ctx = ();
    type Builder = Self;
    fn from_name(id: String, name: String) -> Self {
        DaeNode::new(id, Some(name))
    }
}

// turns a direction vector into an axis and angle (in radians)
// mostly for the purposes of storing a normal into a node's transform
fn vec_to_rotation(vec: &Vec3d, up: UpAxis) -> (Vec3d, f32) {
    let v1 = glm::Vec3::from(*vec).normalize();
    let v2 = glm::Vec3::z_axis();
    let mut cross = v1.cross(&v2);
    if cross.magnitude() < 0.001 {
        cross = *glm::Vec3::y_axis() // forward for DAE
    }
    (Vec3d::from(cross.normalize()).to_coord(up), v1.dot(&v2).acos())
}

// turns properties into a series of dae nodes
fn make_properties_node<N: Node>(ctx: &mut N::Ctx, properties: &str, id: String) -> N {
    let mut node = N::from_id(format!("#{}properties", id));

    for substr in properties.split('\n') {
        node.children()
            .push(N::from_name(format!("#{}:{}", id, substr), format!("{}:{}", id, substr.trim_end())).build(ctx));
        // trailing spaces can mess up parsing, so remove them
    }

    node.build(ctx)
}

fn make_thrusters_node<N: Node>(ctx: &mut N::Ctx, thruster_banks: &[ThrusterBank], up: UpAxis) -> N {
    let mut node = N::from_id("#thrusters".into());

    for (i, bank) in thruster_banks.iter().enumerate() {
        let mut bank_node = N::from_id(format!("#t-bank{}", i));

        for (j, point) in bank.glows.iter().enumerate() {
            let mut point_node = N::from_id(format!("#tb{}-point{}", i, j));
            let radius = point.radius;
            let pos = point.position.to_coord(up);
            point_node.translate(pos.into());
            point_node.rotate(vec_to_rotation(&point.normal, up));
            point_node.scale([radius, radius, radius]);

            bank_node.children().push(point_node.build(ctx));
        }

        if !bank.properties.is_empty() {
            bank_node
                .children()
                .push(make_properties_node(ctx, &bank.properties, format!("tb{}-", i)));
        }

        node.children().push(bank_node.build(ctx));
    }

    node.build(ctx)
}

fn make_paths_node<N: Node>(ctx: &mut N::Ctx, paths: &[Path], up: UpAxis) -> N {
    let mut node = N::from_id("#paths".into());

    for (i, path) in paths.iter().enumerate() {
        let mut path_node = N::from_name(format!("#p{}", i), format!("#path{}", i));

        for (j, point) in path.points.iter().enumerate() {
            let mut point_node = N::from_name(format!("#path{}-{}", i, j), format!("#p{}-point{}", i, j));
            let radius = point.radius;
            let pos = point.position.to_coord(up);
            point_node.translate(pos.into());
            point_node.scale([radius, radius, radius]);

            path_node.children().push(point_node.build(ctx));
        }

        path_node
            .children()
            .push(N::from_name(format!("#p{}-name", i), format!("#p{}-name:{}", i, path.name)).build(ctx));

        path_node
            .children()
            .push(N::from_name(format!("#p{}-parent", i), format!("#p{}-parent:{}", i, path.parent)).build(ctx));

        node.children().push(path_node.build(ctx));
    }

    node.build(ctx)
}

fn make_weapons_node<N: Node>(ctx: &mut N::Ctx, weapons: &[Vec<WeaponHardpoint>], kind: &str, up: UpAxis) -> N {
    let mut node = N::from_id(format!("#{} weapons", kind));

    for (i, bank) in weapons.iter().enumerate() {
        let mut bank_node = N::from_id(format!("#w{}-bank{}", &kind[0..1], i));

        for (j, point) in bank.iter().enumerate() {
            let mut point_node = N::from_id(format!("#w{}b{}-point{}", &kind[0..1], i, j));
            let pos = point.position.to_coord(up);
            point_node.translate(pos.into());
            point_node.rotate(vec_to_rotation(&point.normal.0, up));

            if point.offset != 0.0 {
                point_node.children().push(
                    N::from_name(
                        format!("#w{}b{}-point{}-offset", &kind[0..1], i, j),
                        format!("#w{}b{}-point{}-offset:{}", &kind[0..1], i, j, point.offset),
                    )
                    .build(ctx),
                );
            }

            bank_node.children().push(point_node.build(ctx));
        }

        node.children().push(bank_node.build(ctx));
    }

    node.build(ctx)
}

fn make_docking_bays_node<N: Node>(ctx: &mut N::Ctx, docks: &[Dock], up: UpAxis) -> N {
    let mut node = N::from_id(format!("#docking bays"));

    for (i, dock) in docks.iter().enumerate() {
        let mut bay_node = N::from_id(format!("#bay{}", i));

        let fvec: Vec3 = dock.fvec.0.to_coord(up).into();
        let uvec = dock.uvec.0.to_coord(up).into();
        let mat = nalgebra::Matrix::from_columns(&[fvec.cross(&uvec), fvec, uvec]);
        let mut mat: Mat4x4 = glm::mat3_to_mat4(&mat);
        mat.append_translation_mut(&dock.position.to_coord(up).into());
        bay_node.matrix_transform(mat);

        if dock.path.is_some() {
            bay_node
                .children()
                .push(N::from_name(format!("#d{}-path", i), format!("#d{}-path:{}", i, dock.path.unwrap().0 as u32)).build(ctx));
        }

        if !dock.properties.is_empty() {
            bay_node.children().push(make_properties_node(ctx, &dock.properties, format!("d{}-", i)));
        }

        node.children().push(bay_node.build(ctx));
    }

    node.build(ctx)
}

fn make_glows_node<N: Node>(ctx: &mut N::Ctx, glows: &[GlowPointBank], up: UpAxis) -> N {
    let mut node = N::from_id("#glows".into());

    for (i, glow_bank) in glows.iter().enumerate() {
        let mut bank_node = N::from_name(format!("#g{}", i), format!("#glowbank{}", i));

        for (j, point) in glow_bank.glow_points.iter().enumerate() {
            let mut point_node = N::from_name(
                format!("#g{}-{}", i, j),
                if point.normal.is_null() {
                    format!("#g{}-omnipoint{}", i, j)
                } else {
                    format!("#g{}-point{}", i, j)
                },
            );
            let radius = point.radius;
            let pos = point.position.to_coord(up);
            point_node.translate(pos.into());
            if !point.normal.is_null() {
                point_node.rotate(vec_to_rotation(&point.normal, up));
            }
            point_node.scale([radius, radius, radius]);

            bank_node.children().push(point_node.build(ctx));
        }

        bank_node.children().extend([
            N::from_name(format!("#g{}-type", i), format!("#g{}-type:{}", i, glow_bank.glow_type)).build(ctx),
            N::from_name(format!("#g{}-lod", i), format!("#g{}-lod:{}", i, glow_bank.lod)).build(ctx),
            N::from_name(format!("#g{}-parent", i), format!("#g{}-parent:{}", i, glow_bank.obj_parent.0)).build(ctx),
            N::from_name(format!("#g{}-ontime", i), format!("#g{}-ontime:{}", i, glow_bank.on_time)).build(ctx),
            N::from_name(format!("#g{}-offtime", i), format!("#g{}-offtime:{}", i, glow_bank.off_time)).build(ctx),
            N::from_name(format!("#g{}-disptime", i), format!("#g{}-disptime:{}", i, glow_bank.disp_time)).build(ctx),
        ]);

        if !glow_bank.properties.is_empty() {
            bank_node
                .children()
                .push(make_properties_node(ctx, &glow_bank.properties, format!("g{}-", i)));
        }

        node.children().push(bank_node.build(ctx));
    }

    node.build(ctx)
}

fn make_specials_node<N: Node>(ctx: &mut N::Ctx, special_points: &[SpecialPoint], up: UpAxis) -> N {
    let mut node = N::from_id(format!("#special points"));

    for (i, point) in special_points.iter().enumerate() {
        let mut point_node = N::from_name(format!("#s{}", i), format!("#s{}:{}", i, point.name));

        let radius = point.radius;
        let pos = point.position.to_coord(up);
        point_node.translate(pos.into());
        point_node.scale([radius, radius, radius]);

        if !point.properties.is_empty() {
            point_node
                .children()
                .push(make_properties_node(ctx, &point.properties, format!("s{}-", i)));
        }

        node.children().push(point_node.build(ctx));
    }

    node.build(ctx)
}

fn make_eyes_node<N: Node>(ctx: &mut N::Ctx, eye_points: &[EyePoint], up: UpAxis) -> N {
    let mut node = N::from_id(format!("#eye points"));

    for (i, point) in eye_points.iter().enumerate() {
        let mut point_node = N::from_name(format!("#e{}", i), format!("#e-point{}", i));

        let pos = point.position.to_coord(up);
        point_node.translate(pos.into());
        point_node.rotate(vec_to_rotation(&point.normal.0, up));

        if let Some(id) = point.attached_subobj {
            point_node
                .children()
                .push(N::from_name(format!("#e{}-parent", i), format!("#e{}-parent:{}", i, id.0)).build(ctx));
        }

        node.children().push(point_node.build(ctx));
    }

    node.build(ctx)
}

fn make_visual_center_node<N: Node>(ctx: &mut N::Ctx, visual_center: &Vec3d, up: UpAxis) -> N {
    let mut node = N::from_id(format!("#visual-center"));

    node.translate(visual_center.to_coord(up).into());

    node.build(ctx)
}

fn make_insignia_node(insignia: &Insignia, geometries: &mut Vec<Geometry>, id: usize, up: UpAxis) -> DaeNode {
    let geo_id = format!("insig{}-geometry", id);
    let pos_id = format!("insig{}-geometry-position", id);
    let vert_id = format!("insig{}-geometry-vertex", id);
    let pos_array_id = format!("{}-array", pos_id);

    let mut positions = vec![];
    for vert in &insignia.vertices {
        positions.extend_from_slice(&<[_; 3]>::from(vert.to_coord(up)));
    }

    let mut tricount = 0;
    let mut indices = vec![];

    for (polyvert1, polyvert2, polyvert3) in &insignia.faces {
        indices.push(polyvert1.vertex_id.0 as _);

        // intentional swizzle
        indices.push(polyvert3.vertex_id.0 as _);

        indices.push(polyvert2.vertex_id.0 as _);

        tricount += 1;
    }

    let instance = Instance::<Geometry>::new(Url::Fragment(geo_id.clone()));

    geometries.push(Geometry::new_mesh(
        geo_id,
        vec![Source::new_local(
            pos_id.clone(),
            Param::new_xyz(),
            ArrayElement::Float(FloatArray { id: Some(pos_array_id), val: positions.into() }), // TODO make a new func
        )],
        Vertices::new(vert_id.clone(), vec![Input::new(Semantic::Position, Url::Fragment(pos_id))]),
        vec![Primitive::Triangles(Triangles::new(
            None,
            vec![InputS::new(Semantic::Vertex, Url::Fragment(vert_id), 0, None)],
            tricount,
            indices.into_boxed_slice(),
        ))],
    ));

    let mut node = DaeNode::new(format!("insig{}", id), Some(format!("insignia {}", id)));

    node.instance_geometry.push(instance);
    node.translate(insignia.offset.to_coord(up).into());

    node
}

fn make_shield_node(shield: &ShieldData, geometries: &mut Vec<Geometry>, up: UpAxis) -> DaeNode {
    let geo_id = format!("shield-geometry");
    let pos_id = format!("shield-geometry-position");
    let vert_id = format!("shield-geometry-vertex");
    let pos_array_id = format!("{}-array", pos_id);
    let norm_id = format!("shield-geometry-normal");
    let norm_array_id = format!("{}-array", norm_id);

    let mut positions = vec![];
    for vert in &shield.verts {
        positions.extend_from_slice(&<[_; 3]>::from(vert.to_coord(up)));
    }

    let mut normals = vec![];
    let mut tricount = 0;
    let mut indices = vec![];

    for poly in &shield.polygons {
        normals.extend_from_slice(&<[_; 3]>::from(poly.normal.to_coord(up)));

        indices.push(poly.verts.0 .0 as _);
        indices.push(tricount as _);

        // intentional swizzle
        indices.push(poly.verts.2 .0 as _);
        indices.push(tricount as _);

        indices.push(poly.verts.1 .0 as _);
        indices.push(tricount as _);

        tricount += 1;
    }

    let instance = Instance::<Geometry>::new(Url::Fragment(geo_id.clone()));

    geometries.push(Geometry::new_mesh(
        geo_id,
        vec![
            Source::new_local(
                pos_id.clone(),
                Param::new_xyz(),
                ArrayElement::Float(FloatArray { id: Some(pos_array_id), val: positions.into() }), // TODO make a new func
            ),
            Source::new_local(norm_id.clone(), Param::new_xyz(), ArrayElement::Float(FloatArray { id: Some(norm_array_id), val: normals.into() })),
        ],
        Vertices::new(vert_id.clone(), vec![Input::new(Semantic::Position, Url::Fragment(pos_id))]),
        vec![Primitive::Triangles(Triangles::new(
            None,
            vec![
                InputS::new(Semantic::Vertex, Url::Fragment(vert_id), 0, None),
                InputS::new(Semantic::Normal, Url::Fragment(norm_id), 1, None),
            ],
            tricount,
            indices.into_boxed_slice(),
        ))],
    ));

    let mut node = DaeNode::from_id(String::from("shield"));

    node.instance_geometry.push(instance);

    node
}

fn make_subobj_node(
    up: UpAxis, subobjs: &ObjVec<SubObject>, subobj: &SubObject, turrets: &[Turret], geometries: &mut Vec<Geometry>, materials: &[String],
) -> DaeNode {
    let geo_id = format!("{}-geometry", subobj.name);
    let pos_id = format!("{}-geometry-position", subobj.name);
    let vert_id = format!("{}-geometry-vertex", subobj.name);
    let pos_array_id = format!("{}-array", pos_id);
    let norm_id = format!("{}-geometry-normal", subobj.name);
    let norm_array_id = format!("{}-array", norm_id);
    let uv_id = format!("{}-geometry-uv", subobj.name);
    let uv_array_id = format!("{}-array", uv_id);

    let mut positions = vec![];
    for vert in &subobj.bsp_data.verts {
        positions.extend_from_slice(&<[_; 3]>::from(vert.to_coord(up)));
    }

    let mut normals = vec![];
    for norm in &subobj.bsp_data.norms {
        normals.extend_from_slice(&<[_; 3]>::from(norm.to_coord(up)));
    }

    let mut uv_coords = vec![];
    let mut uv_len = 0;
    let mut prim_elems = vec![(vec![], vec![]); materials.len() + 1];

    for (_, poly) in subobj.bsp_data.collision_tree.leaves() {
        let (vert_count, indices) = &mut prim_elems[poly.texture.0 as usize + 1];
        vert_count.push(poly.verts.len() as u32);
        for vert in poly.verts.iter().rev() {
            indices.push(vert.vertex_id.0 as _);
            indices.push(vert.normal_id.0 as _);
            indices.push(uv_len);
            uv_len += 1;
            uv_coords.push(vert.uv.0);
            uv_coords.push(1. - vert.uv.1);
        }
    }

    let mut instance = Instance::<Geometry>::new(Url::Fragment(geo_id.clone()));
    let bind_materials = prim_elems[1..]
        .iter()
        .zip(materials)
        .filter(|((vcount, _), _)| !vcount.is_empty())
        .map(|(_, mat)| InstanceMaterial::new(mat.to_string(), Url::Fragment(mat.clone()), vec![BindVertexInput::new("UVMap", "TEXCOORD", Some(0))]))
        .collect::<Vec<_>>();
    if !bind_materials.is_empty() {
        instance.data.bind_material = Some(BindMaterial::new(bind_materials));
    }

    geometries.push(Geometry::new_mesh(
        geo_id,
        vec![
            Source::new_local(
                pos_id.clone(),
                Param::new_xyz(),
                ArrayElement::Float(FloatArray { id: Some(pos_array_id), val: positions.into() }), // TODO make a new func
            ),
            Source::new_local(norm_id.clone(), Param::new_xyz(), ArrayElement::Float(FloatArray { id: Some(norm_array_id), val: normals.into() })),
            Source::new_local(uv_id.clone(), Param::new_st(), ArrayElement::Float(FloatArray { id: Some(uv_array_id), val: uv_coords.into() })),
        ],
        Vertices::new(vert_id.clone(), vec![Input::new(Semantic::Position, Url::Fragment(pos_id))]),
        prim_elems
            .into_iter()
            .zip([None].into_iter().chain(materials.iter().map(Some)))
            .filter(|((vcount, _), _)| !vcount.is_empty())
            .map(|((vcount, indices), material)| {
                // TODO maybe triangles sometimes?
                Primitive::PolyList(PolyList::new(
                    material.cloned(),
                    vec![
                        InputS::new(Semantic::Vertex, Url::Fragment(vert_id.clone()), 0, None),
                        InputS::new(Semantic::Normal, Url::Fragment(norm_id.clone()), 1, None),
                        InputS::new(Semantic::TexCoord, Url::Fragment(uv_id.clone()), 2, Some(0)),
                    ],
                    vcount.into_boxed_slice(),
                    indices.into_boxed_slice(),
                ))
            })
            .collect(),
    ));

    let mut node = DaeNode::from_id(subobj.name.clone());
    node.translate(subobj.offset.to_coord(up).into());

    // kind of expensive to do per subobj?
    for (i, turret) in turrets.iter().enumerate() {
        if turret.gun_obj == subobj.obj_id {
            for (j, point) in turret.fire_points.iter().enumerate() {
                let name = if turret.base_obj == subobj.obj_id {
                    format!("#t{}-point{}", i, j)
                } else {
                    format!("#t{}-gun-point{}", i, j)
                };

                let mut gunpoint_node = DaeNode::from_id(name);
                gunpoint_node.translate(point.to_coord(up).into());
                gunpoint_node.rotate(vec_to_rotation(&turret.normal.0, up));
                node.children().push(gunpoint_node);
            }
        }
    }

    if !subobj.properties.is_empty() {
        node.children
            .push(make_properties_node(&mut (), &subobj.properties, format!("{}-", subobj.name)));
    }

    if subobj.rotation_type != Default::default() {
        node.children
            .push(DaeNode::new(format!("{}-mov-type", subobj.name), Some(format!("#{}-mov-type:{}", subobj.name, subobj.rotation_type as i32))));
    }

    if subobj.rotation_axis != Default::default() {
        node.children
            .push(DaeNode::new(format!("{}-mov-axis", subobj.name), Some(format!("#{}-mov-axis:{}", subobj.name, subobj.rotation_axis as i32))));
    }

    if subobj.translation_type != Default::default() {
        node.children
            .push(DaeNode::new(format!("{}-mov-type", subobj.name), Some(format!("#{}-trans-type:{}", subobj.name, subobj.translation_type as i32))));
    }

    if subobj.translation_axis != Default::default() {
        node.children
            .push(DaeNode::new(format!("{}-mov-axis", subobj.name), Some(format!("#{}-trans-axis:{}", subobj.name, subobj.translation_axis as i32))));
    }

    node.instance_geometry.push(instance);
    node.children.extend(
        subobj
            .children
            .iter()
            .map(|&id| make_subobj_node(up, subobjs, &subobjs[id], turrets, geometries, materials)),
    );

    node
}

impl Model {
    pub fn write_dae(&self, w: &mut impl Write) -> Result<(), dae_parser::Error> {
        let mut geometries = vec![];

        let mut nodes = vec![];
        let materials: Vec<String> = self.textures.iter().map(|tex| format!("{}-material", tex)).collect();

        let up = UpAxis::YUp;

        for subobj in &self.sub_objects {
            if subobj.parent.is_none() {
                let mut top_level_node = make_subobj_node(up, &self.sub_objects, subobj, &self.turrets, &mut geometries, &materials);

                for (i, insignia) in self.insignias.iter().enumerate() {
                    if self.get_sobj_detail_level(subobj.obj_id) == Some(insignia.detail_level) {
                        top_level_node.children.push(make_insignia_node(insignia, &mut geometries, i, up))
                    }
                }

                nodes.push(top_level_node);
            }
        }

        if let Some(shield_data) = &self.shield_data {
            nodes.push(make_shield_node(shield_data, &mut geometries, up));
        }

        if !self.thruster_banks.is_empty() {
            nodes.push(make_thrusters_node(&mut (), &self.thruster_banks, up));
        }

        if !self.paths.is_empty() {
            nodes.push(make_paths_node(&mut (), &self.paths, up));
        }

        if !self.primary_weps.is_empty() {
            nodes.push(make_weapons_node(&mut (), &self.primary_weps, "primary", up));
        }

        if !self.secondary_weps.is_empty() {
            nodes.push(make_weapons_node(&mut (), &self.secondary_weps, "secondary", up));
        }

        if !self.docking_bays.is_empty() {
            nodes.push(make_docking_bays_node(&mut (), &self.docking_bays, up));
        }

        if !self.glow_banks.is_empty() {
            nodes.push(make_glows_node(&mut (), &self.glow_banks, up));
        }

        if !self.special_points.is_empty() {
            nodes.push(make_specials_node(&mut (), &self.special_points, up));
        }

        if !self.eye_points.is_empty() {
            nodes.push(make_eyes_node(&mut (), &self.eye_points, up));
        }

        if !self.visual_center.is_null() {
            nodes.push(make_visual_center_node(&mut (), &self.visual_center, up));
        }

        let mut doc = Document::create_now();
        doc.push_library(
            self.textures
                .iter()
                .map(|tex| Effect::shader(format!("{}-effect", tex), Lambert::default()))
                .collect(),
        );

        doc.push_library(
            self.textures
                .iter()
                .map(|tex| Material::new(format!("{}-material", tex), tex.clone(), Url::Fragment(format!("{}-effect", tex))))
                .collect(),
        );

        doc.push_library(geometries);

        let mut scene = VisualScene::new("Scene", None);
        scene.nodes = nodes;
        doc.push_library(vec![scene]);

        doc.scene = Some(Scene::new(Instance::new(Url::Fragment("Scene".to_string()))));

        doc.asset.up_axis = up;

        doc.write_to(w)
    }
}

#[derive(Default)]
struct GltfBuilder {
    root: json::Root,
    buffer: Vec<u8>,
}

fn sanitize_f32(f: f32) -> f32 {
    if f.is_finite() {
        f
    } else {
        0.
    }
}

impl NodeBuilder for json::Node {
    type Ctx = Vec<Self>;
    type Node = Index<Self>;
    fn children(&mut self) -> &mut Vec<Index<Self>> {
        self.children.get_or_insert_with(Default::default)
    }
    fn translate(&mut self, val: [f32; 3]) {
        self.translation = Some(val.map(sanitize_f32))
    }
    fn rotate(&mut self, (Vec3d { x, y, z }, angle): (Vec3d, f32)) {
        let [x, y, z, angle] = [x, y, z, angle].map(sanitize_f32);
        let (sin_a, cos_a) = f32::sin_cos(angle / 2.);
        self.rotation = Some(json::scene::UnitQuaternion([x * sin_a, y * sin_a, z * sin_a, cos_a]));
    }
    fn scale(&mut self, val: [f32; 3]) {
        self.scale = Some(val.map(sanitize_f32))
    }
    fn matrix_transform(&mut self, mat: Mat4x4) {
        self.matrix = Some(Matrix::from(mat.transpose()).0.map(sanitize_f32))
    }
    fn build(self, ctx: &mut Vec<Self>) -> Index<Self> {
        GltfBuilder::push(ctx, self)
    }
}
type NodeIndex = Index<json::Node>;
impl Node for NodeIndex {
    type Ctx = Vec<json::Node>;
    type Builder = json::Node;
    fn from_name(_: String, name: String) -> json::Node {
        Self::from_id(name)
    }
    fn from_id(name: String) -> json::Node {
        json::Node {
            camera: None,
            children: None,
            extensions: Default::default(),
            extras: Default::default(),
            matrix: None,
            mesh: None,
            name: Some(name),
            rotation: None,
            scale: None,
            translation: None,
            skin: None,
            weights: None,
        }
    }
}

impl GltfBuilder {
    fn push<T>(vec: &mut Vec<T>, t: T) -> Index<T> {
        let n = vec.len();
        vec.push(t);
        Index::new(n.try_into().unwrap())
    }

    fn push_buffer_view(&mut self, offset: usize, size: usize, packed: bool, count: usize, target: Target) -> Index<json::buffer::View> {
        Self::push(
            &mut self.root.buffer_views,
            json::buffer::View {
                buffer: json::Index::new(0),
                byte_length: (size * count) as u32,
                byte_offset: Some(offset.try_into().unwrap()),
                byte_stride: if packed { None } else { Some(size as u32) },
                extensions: None,
                extras: Default::default(),
                name: None,
                target: Some(Valid(target)),
            },
        )
    }

    fn push_accessor(
        &mut self, buffer_view: Index<json::buffer::View>, byte_offset: usize, count: usize, component_type: ComponentType,
        type_: json::accessor::Type, bbox: Option<BoundingBox>,
    ) -> Index<json::Accessor> {
        Self::push(
            &mut self.root.accessors,
            json::Accessor {
                buffer_view: Some(buffer_view),
                byte_offset: byte_offset as u32,
                count: count as u32,
                component_type: Valid(json::accessor::GenericComponentType(component_type)),
                extensions: Default::default(),
                extras: Default::default(),
                type_: Valid(type_),
                min: bbox.map(|bbox| json::Value::from(vec![bbox.min.x, bbox.min.y, bbox.min.z])),
                max: bbox.map(|bbox| json::Value::from(vec![bbox.max.x, bbox.max.y, bbox.max.z])),
                name: None,
                normalized: false,
                sparse: None,
            },
        )
    }

    fn push_mesh(&mut self, primitives: Vec<json::mesh::Primitive>) -> Index<json::Mesh> {
        Self::push(
            &mut self.root.meshes,
            json::Mesh {
                extensions: Default::default(),
                extras: Default::default(),
                name: None,
                primitives,
                weights: None,
            },
        )
    }

    fn make_insignia_node(&mut self, insignia: &Insignia, id: usize, up: UpAxis) -> NodeIndex {
        let start = self.buffer.len();
        let view = self.push_buffer_view(start, size_of::<Vec3d>(), true, insignia.vertices.len(), Target::ArrayBuffer);
        let bbox = BoundingBox::from_vectors(insignia.vertices.iter().map(|vert| {
            let vert = vert.to_coord(up);
            vert.write_to(&mut self.buffer).unwrap();
            vert
        }));

        let indices = self.buffer.len();
        let count = 3 * insignia.faces.len();
        let indices = self.push_buffer_view(indices, size_of::<u16>(), true, count, Target::ElementArrayBuffer);
        let indices = self.push_accessor(indices, 0, count, ComponentType::U16, json::accessor::Type::Scalar, None);

        for (polyvert1, polyvert2, polyvert3) in &insignia.faces {
            // intentional swizzle
            (polyvert1.vertex_id.0 as u16).write_to(&mut self.buffer).unwrap();
            (polyvert3.vertex_id.0 as u16).write_to(&mut self.buffer).unwrap();
            (polyvert2.vertex_id.0 as u16).write_to(&mut self.buffer).unwrap();
        }

        let attributes = [(
            Valid(json::mesh::Semantic::Positions),
            self.push_accessor(view, 0, insignia.vertices.len(), ComponentType::F32, json::accessor::Type::Vec3, Some(bbox)),
        )];

        let mut node = NodeIndex::from_id(format!("insignia {}", id));

        node.mesh = Some(self.push_mesh(vec![json::mesh::Primitive {
            attributes: attributes.into_iter().collect(),
            extensions: Default::default(),
            extras: Default::default(),
            indices: Some(indices),
            material: None,
            mode: Valid(json::mesh::Mode::Triangles),
            targets: None,
        }]));

        node.translate(insignia.offset.to_coord(up).into());
        node.build(&mut self.root.nodes)
    }

    fn make_shield_node(&mut self, shield: &ShieldData, up: UpAxis) -> NodeIndex {
        let start = self.buffer.len();
        let count = 3 * shield.polygons.len();
        let view = self.push_buffer_view(start, size_of::<(Vec3d, Vec3d)>(), false, count, Target::ArrayBuffer);

        let mut bbox_pos = BoundingBox::EMPTY;
        let mut bbox_norm = BoundingBox::EMPTY;
        for poly in &shield.polygons {
            let (v1, v2, v3) = poly.verts;
            let v1 = shield.verts[v1.0 as usize].to_coord(up);
            let v2 = shield.verts[v2.0 as usize].to_coord(up);
            let v3 = shield.verts[v3.0 as usize].to_coord(up);
            let normal = poly.normal.to_coord(up);

            // intentional swizzle
            (v1, normal).write_to(&mut self.buffer).unwrap();
            (v3, normal).write_to(&mut self.buffer).unwrap();
            (v2, normal).write_to(&mut self.buffer).unwrap();

            bbox_pos.expand_vec(v1);
            bbox_pos.expand_vec(v2);
            bbox_pos.expand_vec(v3);
            bbox_norm.expand_vec(normal);
        }
        let attributes = [
            (
                Valid(json::mesh::Semantic::Positions),
                self.push_accessor(view, 0, count, ComponentType::F32, json::accessor::Type::Vec3, Some(bbox_pos)),
            ),
            (
                Valid(json::mesh::Semantic::Normals),
                self.push_accessor(view, size_of::<Vec3d>(), count, ComponentType::F32, json::accessor::Type::Vec3, None),
            ),
        ];

        let mut node = NodeIndex::from_id("shield".into());

        node.mesh = Some(self.push_mesh(vec![json::mesh::Primitive {
            attributes: attributes.into_iter().collect(),
            extensions: Default::default(),
            extras: Default::default(),
            indices: None,
            material: None,
            mode: Valid(json::mesh::Mode::Triangles),
            targets: None,
        }]));

        node.build(&mut self.root.nodes)
    }

    fn make_subobj_node(&mut self, subobjs: &ObjVec<SubObject>, subobj: &SubObject, turrets: &[Turret], materials: usize) -> json::Node {
        let mut prim_elems = vec![vec![]; materials];
        for (_, poly) in subobj.bsp_data.collision_tree.leaves() {
            if let [vert1, rest @ ..] = &*poly.verts {
                let tris = &mut prim_elems[poly.texture.0 as usize];
                for verts in rest.windows(2) {
                    // intentional swizzle
                    tris.push([vert1, &verts[1], &verts[0]]);
                }
            }
        }
        let up = UpAxis::YUp;
        let primitives = prim_elems
            .into_iter()
            .enumerate()
            .filter(|(_, vcount)| !vcount.is_empty())
            .map(|(material, prim_elem)| {
                let start = self.buffer.len();
                let count = 3 * prim_elem.len();
                let view = self.push_buffer_view(start, size_of::<(Vec3d, Vec3d, [f32; 2])>(), false, count, Target::ArrayBuffer);
                let mut bbox_pos = BoundingBox::EMPTY;
                for vert in prim_elem.into_iter().flatten() {
                    let position = subobj.bsp_data.verts[vert.vertex_id.0 as usize].to_coord(up);
                    let normal = subobj.bsp_data.norms[vert.normal_id.0 as usize].to_coord(up);
                    bbox_pos.expand_vec(position);
                    (position, normal, vert.uv).write_to(&mut self.buffer).unwrap();
                }
                let attributes = [
                    (
                        Valid(json::mesh::Semantic::Positions),
                        self.push_accessor(view, 0, count, ComponentType::F32, json::accessor::Type::Vec3, Some(bbox_pos)),
                    ),
                    (
                        Valid(json::mesh::Semantic::Normals),
                        self.push_accessor(view, size_of::<Vec3d>(), count, ComponentType::F32, json::accessor::Type::Vec3, None),
                    ),
                    (
                        Valid(json::mesh::Semantic::TexCoords(0)),
                        self.push_accessor(view, 2 * size_of::<Vec3d>(), count, ComponentType::F32, json::accessor::Type::Vec2, None),
                    ),
                ];
                json::mesh::Primitive {
                    attributes: attributes.into_iter().collect(),
                    extensions: Default::default(),
                    extras: Default::default(),
                    indices: None,
                    material: Some(Index::new(material as _)),
                    mode: Valid(json::mesh::Mode::Triangles),
                    targets: None,
                }
            })
            .collect::<Vec<_>>();

        let geo_id = self.push_mesh(primitives);

        let mut node = NodeIndex::from_id(subobj.name.clone());
        node.translate(subobj.offset.to_coord(up).into());

        // kind of expensive to do per subobj?
        for (i, turret) in turrets.iter().enumerate() {
            if turret.gun_obj == subobj.obj_id {
                for (j, point) in turret.fire_points.iter().enumerate() {
                    let name = if turret.base_obj == subobj.obj_id {
                        format!("#t{}-point{}", i, j)
                    } else {
                        format!("#t{}-gun-point{}", i, j)
                    };

                    let mut gunpoint_node = NodeIndex::from_id(name);
                    gunpoint_node.translate(point.to_coord(up).into());
                    gunpoint_node.rotate(vec_to_rotation(&turret.normal.0, up));
                    node.children().push(gunpoint_node.build(&mut self.root.nodes));
                }
            }
        }

        if !subobj.properties.is_empty() {
            node.children()
                .push(make_properties_node(&mut self.root.nodes, &subobj.properties, format!("{}-", subobj.name)));
        }

        if subobj.rotation_type != Default::default() {
            node.children()
                .push(NodeIndex::from_id(format!("#{}-mov-type:{}", subobj.name, subobj.rotation_type as i32)).build(&mut self.root.nodes));
        }

        if subobj.rotation_axis != Default::default() {
            node.children()
                .push(NodeIndex::from_id(format!("#{}-mov-axis:{}", subobj.name, subobj.rotation_axis as i32)).build(&mut self.root.nodes));
        }

        node.mesh = Some(geo_id);
        for &id in &subobj.children {
            node.children().push(
                self.make_subobj_node(subobjs, &subobjs[id], turrets, materials)
                    .build(&mut self.root.nodes),
            );
        }

        node
    }

    pub fn build_gltf(&mut self, model: &Model) {
        let up = UpAxis::YUp;
        self.root.materials.extend(model.textures.iter().map(|tex| {
            let image = json::Image {
                uri: Some(tex.into()),
                buffer_view: Default::default(),
                mime_type: Default::default(),
                name: Default::default(),
                extensions: Default::default(),
                extras: Default::default(),
            };
            let texture = json::Texture {
                name: Some(format!("{}-texture", tex)),
                source: Self::push(&mut self.root.images, image),
                sampler: Default::default(),
                extensions: Default::default(),
                extras: Default::default(),
            };
            json::Material {
                name: Some(tex.clone()),
                pbr_metallic_roughness: json::material::PbrMetallicRoughness {
                    base_color_texture: Some(json::texture::Info {
                        index: Self::push(&mut self.root.textures, texture),
                        tex_coord: 0,
                        extensions: Default::default(),
                        extras: Default::default(),
                    }),
                    ..Default::default()
                },
                ..Default::default()
            }
        }));

        let mut nodes = vec![];

        for subobj in &model.sub_objects {
            if subobj.parent.is_none() {
                let mut top_level_node = self.make_subobj_node(&model.sub_objects, subobj, &model.turrets, model.textures.len());

                for (i, insignia) in model.insignias.iter().enumerate() {
                    if model.get_sobj_detail_level(subobj.obj_id) == Some(insignia.detail_level) {
                        top_level_node.children().push(self.make_insignia_node(insignia, i, up))
                    }
                }

                nodes.push(top_level_node.build(&mut self.root.nodes));
            }
        }

        if let Some(shield_data) = &model.shield_data {
            nodes.push(self.make_shield_node(shield_data, up));
        }

        if !model.thruster_banks.is_empty() {
            nodes.push(make_thrusters_node(&mut self.root.nodes, &model.thruster_banks, up));
        }

        if !model.paths.is_empty() {
            nodes.push(make_paths_node(&mut self.root.nodes, &model.paths, up));
        }

        if !model.primary_weps.is_empty() {
            nodes.push(make_weapons_node(&mut self.root.nodes, &model.primary_weps, "primary", up));
        }

        if !model.secondary_weps.is_empty() {
            nodes.push(make_weapons_node(&mut self.root.nodes, &model.secondary_weps, "secondary", up));
        }

        if !model.docking_bays.is_empty() {
            nodes.push(make_docking_bays_node(&mut self.root.nodes, &model.docking_bays, up));
        }

        if !model.glow_banks.is_empty() {
            nodes.push(make_glows_node(&mut self.root.nodes, &model.glow_banks, up));
        }

        if !model.special_points.is_empty() {
            nodes.push(make_specials_node(&mut self.root.nodes, &model.special_points, up));
        }

        if !model.eye_points.is_empty() {
            nodes.push(make_eyes_node(&mut self.root.nodes, &model.eye_points, up));
        }

        if !model.visual_center.is_null() {
            nodes.push(make_visual_center_node(&mut self.root.nodes, &model.visual_center, up));
        }

        self.root.scene = Some(GltfBuilder::push(
            &mut self.root.scenes,
            json::Scene {
                extensions: None,
                extras: Default::default(),
                name: None,
                nodes,
            },
        ));

        self.root.buffers.push(json::Buffer {
            byte_length: self.buffer.len() as u32,
            name: None,
            uri: None,
            extensions: Default::default(),
            extras: Default::default(),
        });
    }
}

impl Model {
    pub fn write_gltf(&self, writer: impl Write, binary: bool) -> Result<(), gltf::Error> {
        let mut builder = GltfBuilder::default();
        builder.build_gltf(self);
        if binary {
            let json_string = json::serialize::to_string(&builder.root)?;
            let mut json_offset = json_string.len() as u32;
            json_offset += json_offset.wrapping_neg() % 4;
            align_buf(&mut builder.buffer).unwrap();
            let glb = gltf::binary::Glb {
                header: gltf::binary::Header {
                    magic: *b"glTF",
                    version: 2,
                    length: json_offset + builder.root.buffers[0].byte_length,
                },
                bin: Some(Cow::Borrowed(&builder.buffer)),
                json: Cow::Owned(json_string.into_bytes()),
            };
            glb.to_writer(writer)
        } else {
            builder.root.buffers[0].uri = Some(format!(
                "data:application/octet-stream;base64,{}",
                base64::display::Base64Display::with_config(&builder.buffer, base64::STANDARD)
            ));
            json::serialize::to_writer_pretty(writer, &builder.root)?;
            Ok(())
        }
    }
}
