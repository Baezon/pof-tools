use std::convert::TryFrom;
use std::fmt::{Debug, Display};
use std::io::{self, Write};
use std::ops::{Add, AddAssign, Mul, MulAssign, Sub, SubAssign};
use std::str::FromStr;

use byteorder::{WriteBytesExt, LE};
use nalgebra_glm::Mat4;
extern crate nalgebra_glm as glm;

use crate::write::Serialize;

macro_rules! id_type {
    ($name:ident, $type:ty) => {
        #[derive(Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $name(pub $type);
        impl Debug for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.write_fmt(format_args!("{}", &self.0))
            }
        }
        impl Serialize for $name {
            fn write_to(&self, w: &mut impl Write) -> io::Result<()> {
                self.0.write_to(w)
            }
        }
    };
}

id_type! {ObjectId, u32}
id_type! {TextureId, u32}
id_type! {VertexId, u16}
id_type! {NormalId, u16}
id_type! {PolygonId, u32}
id_type! {PathId, u32}

macro_rules! mk_struct {
    ($($(#[$meta:meta])* pub struct $tyname:ident { $(pub $name:ident: $ty:ty,)* })*) => {
        $(
            $(#[$meta])*
            pub struct $tyname {
                $(pub $name: $ty,)*
            }

            impl Serialize for $tyname {
                fn write_to(&self, w: &mut impl Write) -> io::Result<()> {
                    $(self.$name.write_to(w)?;)*
                    Ok(())
                }
            }
        )*
    };
}

mk_struct! {
    #[derive(Clone, Copy, Default, PartialEq)]
    pub struct Vec3d {
        pub x: f32,
        pub y: f32,
        pub z: f32,
    }
}
impl Debug for Vec3d {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {}, {})", &self.x, &self.y, &self.z)
    }
}
impl From<[f32; 3]> for Vec3d {
    fn from([x, y, z]: [f32; 3]) -> Self {
        Vec3d { x, y, z }
    }
}
impl From<(f32, f32, f32)> for Vec3d {
    fn from((x, y, z): (f32, f32, f32)) -> Self {
        Vec3d { x, y, z }
    }
}
impl From<Vec3d> for nalgebra_glm::Vec3 {
    fn from(Vec3d { x, y, z }: Vec3d) -> Self {
        glm::vec3(x, y, z)
    }
}
impl From<nalgebra_glm::Vec3> for Vec3d {
    fn from(vec: nalgebra_glm::Vec3) -> Self {
        <[f32; 3]>::from(vec).into()
    }
}
impl From<Vec3d> for nalgebra::Point3<f32> {
    fn from(Vec3d { x, y, z }: Vec3d) -> Self {
        [x, y, z].into()
    }
}
impl From<nalgebra::Point3<f32>> for Vec3d {
    fn from(vec: nalgebra::Point3<f32>) -> Self {
        <[f32; 3]>::from(vec).into()
    }
}
impl FromStr for Vec3d {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut iter = s.split(",");

        let vec = Vec3d {
            x: iter.next().ok_or(())?.trim().parse().map_err(|_| ())?,
            y: iter.next().ok_or(())?.trim().parse().map_err(|_| ())?,
            z: iter.next().ok_or(())?.trim().parse().map_err(|_| ())?,
        };

        if iter.next().is_some() {
            Err(())
        } else {
            Ok(vec)
        }
    }
}
impl Display for Vec3d {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}, {}, {}", &self.x, &self.y, &self.z)
    }
}
impl Vec3d {
    pub const ZERO: Vec3d = Vec3d { x: 0.0, y: 0.0, z: 0.0 };
    pub fn to_tuple(self) -> (f32, f32, f32) {
        (self.x, self.y, self.z)
    }
    pub fn magnitude(self) -> f32 {
        f32::sqrt(self.x * self.x + self.y * self.y + self.z * self.z)
    }
}
impl Add for Vec3d {
    type Output = Vec3d;

    fn add(self, rhs: Self) -> Self::Output {
        Vec3d { x: self.x + rhs.x, y: self.y + rhs.y, z: self.z + rhs.z }
    }
}
impl AddAssign for Vec3d {
    fn add_assign(&mut self, rhs: Vec3d) {
        self.x += rhs.x;
        self.y += rhs.y;
        self.z += rhs.z;
    }
}
impl Sub for Vec3d {
    type Output = Vec3d;

    fn sub(self, rhs: Self) -> Self::Output {
        Vec3d { x: self.x - rhs.x, y: self.y - rhs.y, z: self.z - rhs.z }
    }
}
impl SubAssign for Vec3d {
    fn sub_assign(&mut self, rhs: Vec3d) {
        self.x -= rhs.x;
        self.y -= rhs.y;
        self.z -= rhs.z;
    }
}
impl MulAssign<f32> for Vec3d {
    fn mul_assign(&mut self, rhs: f32) {
        self.x *= rhs;
        self.y *= rhs;
        self.z *= rhs;
    }
}
impl Mul<f32> for Vec3d {
    type Output = Vec3d;

    fn mul(self, rhs: f32) -> Vec3d {
        Vec3d { x: self.x * rhs, y: self.y * rhs, z: self.z * rhs }
    }
}
impl Mul<Vec3d> for &Mat4 {
    type Output = Vec3d;

    fn mul(self, rhs: Vec3d) -> Self::Output {
        self.transform_point(&rhs.into()).into()
    }
}

mk_struct! {
    #[derive(Debug, Default)]
    pub struct Mat3d {
        pub rvec: Vec3d,
        pub uvec: Vec3d,
        pub fvec: Vec3d,
    }

    #[derive(Default, Clone, Copy)]
    pub struct BBox {
        pub min: Vec3d,
        pub max: Vec3d,
    }
}
impl Debug for BBox {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}, {:?}", &self.min, &self.max))
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BspLightKind {
    Muzzle = 1,
    Thruster = 2,
}
impl Serialize for BspLightKind {
    fn write_to(&self, w: &mut impl Write) -> io::Result<()> {
        w.write_u32::<LE>(*self as u32)
    }
}

mk_struct! {
    #[derive(Debug)]
    pub struct BspLight {
        pub location: Vec3d,
        pub kind: BspLightKind,
    }

    #[derive(Debug, Clone)]
    pub struct EyePoint {
        pub attached_subobj: ObjectId,
        pub offset: Vec3d,
        pub normal: Vec3d,
    }
}
impl Default for EyePoint {
    fn default() -> Self {
        Self {
            attached_subobj: ObjectId(0),
            offset: Default::default(),
            normal: Vec3d { x: 0.0, y: 0.0, z: 1.0 },
        }
    }
}

mk_struct! {
    #[derive(Debug, Clone, Default)]
    pub struct PathPoint {
        pub position: Vec3d,
        pub radius: f32,
        pub turrets: Vec<ObjectId>,
    }

    #[derive(Clone, Default)]
    pub struct Path {
        pub name: String,
        pub parent: String,
        pub points: Vec<PathPoint>,
    }
}
impl Debug for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Path")
            .field("name", &self.name)
            .field("parent", &self.parent)
            .field("points", &self.points.len())
            .finish()
    }
}

#[derive(Debug, Clone)]
pub struct PolyVertex<T = NormalId> {
    pub vertex_id: VertexId,
    pub normal_id: T,
    pub uv: (f32, f32),
}
impl Serialize for PolyVertex<()> {
    fn write_to(&self, w: &mut impl Write) -> io::Result<()> {
        self.vertex_id.write_to(w)?;
        0_u16.write_to(w)?;
        self.uv.write_to(w)
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Texturing {
    Flat(Color),
    Texture(TextureId),
}
impl Serialize for Texturing {
    fn write_to(&self, w: &mut impl Write) -> io::Result<()> {
        match self {
            Texturing::Flat(color) => color.write_to(w),
            Texturing::Texture(tmap) => tmap.write_to(w),
        }
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub struct Color {
    pub red: u8,
    pub green: u8,
    pub blue: u8,
}
impl Serialize for Color {
    fn write_to(&self, w: &mut impl Write) -> io::Result<()> {
        w.write_u8(self.red)?;
        w.write_u8(self.green)?;
        w.write_u8(self.blue)?;
        w.write_u8(0)
    }
}

#[derive(Clone, Copy)]
pub struct ShieldPolygon {
    pub normal: Vec3d,
    pub verts: (VertexId, VertexId, VertexId),
    pub neighbors: (PolygonId, PolygonId, PolygonId),
}
impl Debug for ShieldPolygon {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "ShieldPolygon: {:?}, verts: {:?}, {:?}, {:?}, neighbors: {:?}, {:?}, {:?}",
            self.normal, self.verts.0, self.verts.1, self.verts.2, self.neighbors.0, self.neighbors.1, self.neighbors.2
        )
    }
}
impl Serialize for ShieldPolygon {
    fn write_to(&self, w: &mut impl Write) -> io::Result<()> {
        let ShieldPolygon { normal, verts: (x, y, z), neighbors } = self;
        normal.write_to(w)?;
        (x.0 as u32, y.0 as u32, z.0 as u32).write_to(w)?;
        neighbors.write_to(w)
    }
}

#[derive(Debug)]
pub enum ShieldNode {
    Split {
        bbox: BBox,
        front: Box<ShieldNode>,
        back: Box<ShieldNode>,
    },
    Leaf {
        bbox: Option<BBox>,
        poly_list: Vec<PolygonId>,
    },
}
impl ShieldNode {
    pub(crate) const SPLIT: u32 = 0;
    pub(crate) const LEAF: u32 = 1;
}
impl Serialize for ShieldNode {
    fn write_to(&self, w: &mut impl Write) -> io::Result<()> {
        let mut buf = vec![];

        crate::write::write_shield_node(&mut buf, self)?;

        w.write_u32::<LE>((buf.len()) as u32)?;
        w.write_all(&buf)
    }
}

mk_struct! {
    #[derive(Debug, Clone)]
    pub struct SpecialPoint {
        pub name: String,
        pub properties: String,
        pub position: Vec3d,
        pub radius: f32,
    }
}
impl Default for SpecialPoint {
    fn default() -> Self {
        Self {
            name: Default::default(),
            properties: Default::default(),
            position: Default::default(),
            radius: 1.0,
        }
    }
}

mk_struct! {
    #[derive(Debug, Clone)]
    pub struct WeaponHardpoint {
        pub position: Vec3d,
        pub normal: Vec3d,
        pub offset: f32, //TODO version dependence
    }
}
impl Default for WeaponHardpoint {
    fn default() -> Self {
        Self {
            position: Default::default(),
            normal: Vec3d { x: 0.0, y: 0.0, z: 1.0 },
            offset: Default::default(),
        }
    }
}

mk_struct! {
    #[derive(Debug, Clone)]
    pub struct ThrusterGlow {
        pub position: Vec3d,
        pub normal: Vec3d,
        pub radius: f32,
    }
}
impl Default for ThrusterGlow {
    fn default() -> Self {
        Self {
            position: Default::default(),
            normal: Vec3d { x: 0.0, y: 0.0, z: -1.0 },
            radius: 1.0,
        }
    }
}

mk_struct! {
    #[derive(Debug, Clone, Copy)]
    pub struct DockingPoint {
        pub position: Vec3d,
        pub normal: Vec3d,
    }
}
impl Default for DockingPoint {
    fn default() -> Self {
        Self {
            position: Default::default(),
            normal: Vec3d { x: 0.0, y: 1.0, z: 0.0 },
        }
    }
}

mk_struct! {
    #[derive(Debug, Clone)]
    pub struct GlowPoint {
        pub position: Vec3d,
        pub normal: Vec3d,
        pub radius: f32,
    }
}
impl Default for GlowPoint {
    fn default() -> Self {
        Self {
            position: Default::default(),
            normal: Default::default(),
            radius: 1.0,
        }
    }
}

mk_struct! {
    #[derive(Debug, Default)]
    pub struct ObjHeader {
        pub max_radius: f32,
        pub obj_flags: u32,
        pub num_subobjects: u32,

        pub bounding_box: BBox,

        pub detail_levels: Vec<ObjectId>,

        pub mass: f32,
        pub center_of_mass: Vec3d,
        pub moment_of_inertia: Mat3d,

        pub cross_sections: Vec<(f32, f32)>, // depth, radius

        pub bsp_lights: Vec<BspLight>,
    }
}

#[derive(Debug)]
pub struct ShieldData {
    pub verts: Vec<Vec3d>,
    pub polygons: Vec<ShieldPolygon>,
    pub collision_tree: Option<ShieldNode>,
}

#[derive(Debug)]
pub struct Polygon {
    pub normal: Vec3d,
    pub center: Vec3d,
    pub radius: f32,
    pub texture: Texturing,
    pub verts: Vec<PolyVertex>,
}

#[derive(Debug)]
pub enum BspNode {
    Split {
        normal: Vec3d,
        point: Vec3d,
        bbox: BBox,
        front: Box<BspNode>,
        back: Box<BspNode>,
    },
    Leaf {
        bbox: BBox,
        poly_list: Vec<Polygon>,
    },
}
impl BspNode {
    pub fn leaves(&self) -> BspNodeIter<'_> {
        BspNodeIter { stack: vec![self] }
    }
}

pub struct BspNodeIter<'a> {
    stack: Vec<&'a BspNode>,
}

impl<'a> Iterator for BspNodeIter<'a> {
    type Item = (&'a BBox, &'a Vec<Polygon>);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.stack.pop()? {
                BspNode::Split { front, back, .. } => {
                    self.stack.push(back);
                    self.stack.push(front);
                }
                BspNode::Leaf { bbox, poly_list } => {
                    return Some((bbox, poly_list));
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct BspData {
    pub verts: Vec<Vec3d>,
    pub norms: Vec<Vec3d>,
    pub collision_tree: BspNode,
}
impl BspData {
    pub(crate) const ENDOFBRANCH: u32 = 0;
    pub(crate) const DEFFPOINTS: u32 = 1;
    pub(crate) const FLATPOLY: u32 = 2;
    pub(crate) const TMAPPOLY: u32 = 3;
    pub(crate) const SORTNORM: u32 = 4;
    pub(crate) const BOUNDBOX: u32 = 5;
}
impl Serialize for BspData {
    fn write_to(&self, w: &mut impl Write) -> io::Result<()> {
        w.write_u32::<LE>(0)?;

        let mut buf = vec![];

        crate::write::write_bsp_data(&mut buf, self)?;

        w.write_u32::<LE>((buf.len()) as u32)?;
        w.write_all(&buf)
    }
}

#[derive(Debug, Clone, Default)]
pub struct ThrusterBank {
    pub properties: String,
    pub glows: Vec<ThrusterGlow>,
}
impl Serialize for ThrusterBank {
    fn write_to(&self, w: &mut impl Write) -> io::Result<()> {
        (self.glows.len() as u32).write_to(w)?;
        self.properties.write_to(w)?;
        for glow in &self.glows {
            glow.write_to(w)?;
        }
        Ok(())
    }
}

macro_rules! mk_enumeration {
    ($($(#[$meta:meta])* pub enum $tyname:ident($base:ty) {
        $($(#[$doc:meta])* $name:ident = $n:literal,)*
    })*) => {
        $(
            $(#[$meta])*
            pub enum $tyname {
                $($(#[$doc])* $name = $n,)*
            }

            impl Serialize for $tyname {
                fn write_to(&self, w: &mut impl Write) -> io::Result<()> {
                    (*self as $base).write_to(w)
                }
            }

            impl TryFrom<$base> for $tyname {
                type Error = ();

                fn try_from(value: $base) -> Result<Self, Self::Error> {
                    match value {
                        $($n => Ok(Self::$name),)*
                        _ => Err(()),
                    }
                }
            }
            impl From<$tyname> for $base {
                fn from(value: $tyname) -> Self {
                    match value {
                        $($tyname::$name => $n,)*
                    }
                }
            }
        )*
    };
}

mk_enumeration! {
    #[derive(Debug, Clone, Copy)]
    pub enum SubsysMovementType(i32) {
        // no idea what any of these are, just copied from the source
        NONE = -1,
        POS = 0,
        ROT = 1,
        ROTSPECIAL = 2,
        TRIGGERED = 3,
        INTRINSICROTATE = 4,
    }
}
impl Default for SubsysMovementType {
    fn default() -> Self {
        Self::NONE
    }
}

mk_enumeration! {
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub enum SubsysMovementAxis(i32) {
        NONE = -1,
        XAXIS = 0,
        ZAXIS = 1,
        YAXIS = 2,
        OTHER = 3,
    }
}
impl Default for SubsysMovementAxis {
    fn default() -> Self {
        Self::NONE
    }
}

#[derive(Debug)]
pub struct SubObject {
    pub obj_id: ObjectId,
    pub radius: f32,
    pub parent: Option<ObjectId>,
    pub offset: Vec3d,
    pub geo_center: Vec3d,
    pub bbox: BBox,
    pub name: String,
    pub properties: String,
    pub movement_type: SubsysMovementType,
    pub movement_axis: SubsysMovementAxis,
    pub bsp_data: BspData,

    pub children: Vec<ObjectId>,
    pub is_debris_model: bool,
}
impl SubObject {
    pub fn is_destroyed_model(&self) -> bool {
        self.name.to_lowercase().ends_with("-destroyed")
    }
}
impl Serialize for SubObject {
    fn write_to(&self, w: &mut impl Write) -> io::Result<()> {
        self.obj_id.write_to(w)?;
        self.radius.write_to(w)?;
        self.parent.unwrap_or(ObjectId(u32::MAX)).write_to(w)?;
        self.offset.write_to(w)?;
        self.geo_center.write_to(w)?;
        self.bbox.write_to(w)?;
        self.name.write_to(w)?;
        self.properties.write_to(w)?;
        self.movement_type.write_to(w)?;
        self.movement_axis.write_to(w)?;
        self.bsp_data.write_to(w)
    }
}

#[derive(Debug, Clone, Default)]
pub struct Dock {
    pub properties: String,
    pub path: Option<PathId>,
    pub points: Vec<DockingPoint>,
}
impl Serialize for Dock {
    fn write_to(&self, w: &mut impl Write) -> io::Result<()> {
        self.properties.write_to(w)?;
        match self.path {
            None => 0_u32.write_to(w)?,
            Some(x) => [x].write_to(w)?,
        }
        self.points.write_to(w)
    }
}

mk_struct! {
    #[derive(Clone, Default)]
    pub struct Turret {
        pub base_obj: ObjectId,
        pub gun_obj: ObjectId,
        pub normal: Vec3d,
        pub fire_points: Vec<Vec3d>,
    }
}

impl Debug for Turret {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Turret")
            .field("base_obj", &self.base_obj)
            .field("gun_obj", &self.gun_obj)
            .field("normal", &self.normal)
            .field("fire_points", &self.fire_points.len())
            .finish()
    }
}

#[derive(Debug)]
pub struct Insignia {
    pub detail_level: u32,
    pub vertices: Vec<Vec3d>,
    pub offset: Vec3d,
    pub faces: Vec<(PolyVertex<()>, PolyVertex<()>, PolyVertex<()>)>, // any number of 3-tuples of polyvertices (without normals hence the <()>)
}

impl Serialize for Insignia {
    fn write_to(&self, w: &mut impl Write) -> io::Result<()> {
        self.detail_level.write_to(w)?;
        (self.faces.len() as u32).write_to(w)?;
        self.vertices.write_to(w)?;
        self.offset.write_to(w)?;
        for face in &self.faces {
            face.write_to(w)?;
        }
        Ok(())
    }
}

#[derive(Debug, Default, Clone)]
pub struct GlowPointBank {
    pub disp_time: i32,
    pub on_time: u32,
    pub off_time: u32,
    pub obj_parent: ObjectId,
    pub lod: u32,
    pub glow_type: u32,
    pub properties: String,
    pub glow_points: Vec<GlowPoint>,
}
impl Serialize for GlowPointBank {
    fn write_to(&self, w: &mut impl Write) -> io::Result<()> {
        self.disp_time.write_to(w)?;
        self.on_time.write_to(w)?;
        self.off_time.write_to(w)?;
        self.obj_parent.write_to(w)?;
        self.lod.write_to(w)?;
        self.glow_type.write_to(w)?;
        (self.glow_points.len() as u32).write_to(w)?;
        self.properties.write_to(w)?;
        for glow in &self.glow_points {
            glow.write_to(w)?;
        }
        Ok(())
    }
}

mk_enumeration! {
    #[derive(PartialOrd, Ord, PartialEq, Eq, Debug, Clone, Copy)]
    pub enum Version(i32) {
        V21_16 = 2116,
        V21_17 = 2117,
        V22_00 = 2200,
        V22_01 = 2201,
    }
}

impl Version {
    pub const LATEST: Version = Self::V22_01;
}

#[derive(Debug, Default)]
pub struct Model {
    pub header: ObjHeader,
    pub sub_objects: Vec<SubObject>,
    pub textures: Vec<String>,
    pub paths: Vec<Path>,
    pub special_points: Vec<SpecialPoint>,
    pub eye_points: Vec<EyePoint>,
    pub primary_weps: Vec<Vec<WeaponHardpoint>>,
    pub secondary_weps: Vec<Vec<WeaponHardpoint>>,
    pub turrets: Vec<Turret>,
    pub thruster_banks: Vec<ThrusterBank>,
    pub glow_banks: Vec<GlowPointBank>,
    pub auto_center: Vec3d,
    pub comments: String,
    pub docking_bays: Vec<Dock>,
    pub insignias: Vec<Insignia>,
    pub shield_data: Option<ShieldData>,
}
impl Model {
    pub fn get_total_subobj_offset(&self, mut id: ObjectId) -> Vec3d {
        let mut out = Vec3d::ZERO;
        loop {
            let subobj = &self.sub_objects[id.0 as usize];
            out += subobj.offset;
            if let Some(parent) = subobj.parent {
                id = parent;
            } else {
                break out;
            }
        }
    }

    // see if maybe_anscestor is actually an anscestor of obj_id in the subobject hierarchy
    pub fn is_obj_id_ancestor(&self, obj_id: ObjectId, maybe_ancestor: ObjectId) -> bool {
        if obj_id == maybe_ancestor {
            return true;
        }

        let mut sub_obj_parent = self.sub_objects[obj_id.0 as usize].parent;
        loop {
            if sub_obj_parent == Some(maybe_ancestor) {
                return true;
            } else if sub_obj_parent.is_none() {
                return false;
            }
            sub_obj_parent = self.sub_objects[sub_obj_parent.unwrap().0 as usize].parent;
        }
    }

    pub fn get_subobj_names(&self) -> Vec<String> {
        let mut ret = vec![];
        for subobj in &self.sub_objects {
            ret.push(subobj.name.clone());
        }
        ret
    }
}
