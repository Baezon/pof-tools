use std::cell::Cell;
use std::convert::TryFrom;
use std::fmt::{Debug, Display};
use std::io::{self, Write};
use std::ops::{Add, AddAssign, Deref, DerefMut, Div, DivAssign, Index, IndexMut, Mul, MulAssign, Neg, Sub, SubAssign};
use std::path::PathBuf;
use std::str::FromStr;

use byteorder::{WriteBytesExt, LE};
use glm::{Mat3x3, TMat4, Vec3};
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
id_type! {VertexId, u32}
id_type! {NormalId, u32}
id_type! {PolygonId, u32}
id_type! {PathId, u32}

// what, a global?? in rust?????
// this is how the current version is kept track of while writing pof to disk
// much easier than having to pass around a version to every Serialize implementation despite it mattering in like 1% of cases
thread_local! {
    pub(crate) static VERSION: Cell<Version> = Cell::new(Version::default());
}
macro_rules! get_version {
    () => {
        VERSION.with(|f| f.get())
    };
}

// like a regular vector, but indexed with ObjectIds only, for some safety
#[derive(Debug)]
pub struct ObjVec<T>(pub Vec<T>);
impl<T> Index<ObjectId> for ObjVec<T> {
    type Output = T;

    fn index(&self, index: ObjectId) -> &Self::Output {
        &self.0[index.0 as usize]
    }
}
impl<T> IndexMut<ObjectId> for ObjVec<T> {
    fn index_mut(&mut self, index: ObjectId) -> &mut Self::Output {
        &mut self.0[index.0 as usize]
    }
}
impl<'a, T> IntoIterator for &'a ObjVec<T> {
    type Item = &'a T;

    type IntoIter = std::slice::Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}
impl<T> Default for ObjVec<T> {
    fn default() -> Self {
        Self(Default::default())
    }
}
impl<T> ObjVec<T> {
    fn iter(&self) -> std::slice::Iter<'_, T> {
        self.0.iter()
    }
}
impl<T> Deref for ObjVec<T> {
    type Target = Vec<T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl<T> DerefMut for ObjVec<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

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

#[derive(Clone, Copy)]
pub enum Axis {
    X,
    Y,
    Z,
}

const ALL_AXES: [Axis; 3] = [Axis::X, Axis::Y, Axis::Z];

mk_struct! {
    #[derive(Clone, Copy, Default)]
    pub struct Vec3d {
        pub x: f32,
        pub y: f32,
        pub z: f32,
    }
}
impl Eq for Vec3d {}
impl PartialEq for Vec3d {
    // NaN == NaN, fuck you
    fn eq(&self, other: &Self) -> bool {
        self.x.to_bits() == other.x.to_bits() && self.y.to_bits() == other.y.to_bits() && self.z.to_bits() == other.z.to_bits()
    }
}
impl std::hash::Hash for Vec3d {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.x.to_bits().hash(state);
        self.y.to_bits().hash(state);
        self.z.to_bits().hash(state);
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
impl From<Vec3d> for [f32; 3] {
    fn from(Vec3d { x, y, z }: Vec3d) -> Self {
        [x, y, z]
    }
}
impl From<(f32, f32, f32)> for Vec3d {
    fn from((x, y, z): (f32, f32, f32)) -> Self {
        Vec3d { x, y, z }
    }
}
impl From<Vec3d> for Vec3 {
    fn from(Vec3d { x, y, z }: Vec3d) -> Self {
        glm::vec3(x, y, z)
    }
}
impl From<Vec3> for Vec3d {
    fn from(vec: Vec3) -> Self {
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
        let mut iter = s.split(',');

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
    pub const fn new(x: f32, y: f32, z: f32) -> Self {
        Vec3d { x, y, z }
    }
    pub fn to_tuple(self) -> (f32, f32, f32) {
        (self.x, self.y, self.z)
    }
    pub fn magnitude(self) -> f32 {
        f32::sqrt(self.x * self.x + self.y * self.y + self.z * self.z)
    }
    pub fn normalize(self) -> Vec3d {
        let mag = self.magnitude();
        self * (1.0 / mag)
    }
    pub fn is_null(self) -> bool {
        self.x.abs() <= 0.000001 && self.y.abs() <= 0.000001 && self.z.abs() <= 0.000001
    }
    pub fn average(iter: impl Iterator<Item = Self>) -> Vec3d {
        let mut out = Vec3d::ZERO;
        let mut n = 0;

        for vec in iter {
            out += vec;
            n += 1;
        }

        out /= n as f32;
        out
    }

    // intentional swizzle
    pub fn flip_y_z(&self) -> Vec3d {
        Vec3d { x: self.x, y: self.z, z: self.y }
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
impl DivAssign<f32> for Vec3d {
    fn div_assign(&mut self, rhs: f32) {
        self.x /= rhs;
        self.y /= rhs;
        self.z /= rhs;
    }
}
impl Div<f32> for Vec3d {
    type Output = Vec3d;

    fn div(self, rhs: f32) -> Vec3d {
        Vec3d { x: self.x / rhs, y: self.y / rhs, z: self.z / rhs }
    }
}
impl Neg for Vec3d {
    type Output = Vec3d;

    fn neg(self) -> Self::Output {
        Vec3d { x: -self.x, y: -self.y, z: -self.z }
    }
}
impl Index<Axis> for Vec3d {
    type Output = f32;

    fn index(&self, index: Axis) -> &Self::Output {
        match index {
            Axis::X => &self.x,
            Axis::Y => &self.y,
            Axis::Z => &self.z,
        }
    }
}
impl IndexMut<Axis> for Vec3d {
    fn index_mut(&mut self, index: Axis) -> &mut Self::Output {
        match index {
            Axis::X => &mut self.x,
            Axis::Y => &mut self.y,
            Axis::Z => &mut self.z,
        }
    }
}

mk_struct! {
    #[derive(Debug, Default, Copy, Clone)]
    pub struct Mat3d {
        pub rvec: Vec3d,
        pub uvec: Vec3d,
        pub fvec: Vec3d,
    }
}
impl From<Mat3d> for glm::Mat3x3 {
    fn from(Mat3d { rvec, uvec, fvec }: Mat3d) -> Self {
        glm::Mat3x3::from_columns(&[rvec.into(), uvec.into(), fvec.into()])
    }
}
impl From<glm::Mat3x3> for Mat3d {
    fn from(mat: glm::Mat3x3) -> Self {
        Mat3d {
            rvec: Vec3::from(mat.column(0)).into(),
            uvec: Vec3::from(mat.column(1)).into(),
            fvec: Vec3::from(mat.column(2)).into(),
        }
    }
}
impl MulAssign<f32> for Mat3d {
    fn mul_assign(&mut self, rhs: f32) {
        self.rvec *= rhs;
        self.uvec *= rhs;
        self.fvec *= rhs;
    }
}

#[derive(Debug, Clone)]
pub struct NormalVec3(pub Vec3d);

impl Default for NormalVec3 {
    fn default() -> Self {
        Self(Vec3d { x: 0.0, y: 0.0, z: 1.0 })
    }
}

impl TryFrom<Vec3d> for NormalVec3 {
    type Error = ();

    fn try_from(value: Vec3d) -> Result<Self, Self::Error> {
        Ok(Self(Vec3::from(value).try_normalize(1e-6).ok_or(())?.into()))
    }
}

impl TryFrom<Vec3> for NormalVec3 {
    type Error = ();

    fn try_from(value: Vec3) -> Result<Self, Self::Error> {
        Vec3d::from(value).try_into()
    }
}

impl FromStr for NormalVec3 {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.parse::<Vec3d>()?.try_into()
    }
}

impl Mat3d {
    pub const IDENTITY: Mat3d = Mat3d {
        rvec: Vec3d::new(1., 0., 0.),
        uvec: Vec3d::new(0., 1., 0.),
        fvec: Vec3d::new(0., 0., 1.),
    };

    pub fn add_point_mass_moi(&mut self, pos: Vec3d) {
        self.rvec.x += pos.y * pos.y + pos.z * pos.z;
        self.rvec.y -= pos.x * pos.y;
        self.rvec.z -= pos.x * pos.z;
        self.uvec.x -= pos.x * pos.y;
        self.uvec.y += pos.x * pos.x + pos.z * pos.z;
        self.uvec.z -= pos.y * pos.z;
        self.fvec.x -= pos.x * pos.z;
        self.fvec.y -= pos.y * pos.z;
        self.fvec.z += pos.x * pos.x + pos.y * pos.y;
    }
}

mk_struct! {
    #[derive(Default, Clone, Copy)]
    pub struct BoundingBox {
        pub min: Vec3d,
        pub max: Vec3d,
    }
}
impl Debug for BoundingBox {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}, {:?}", &self.min, &self.max))
    }
}
impl BoundingBox {
    pub fn volume(&self) -> f32 {
        (self.max.x - self.min.x) * (self.max.y - self.min.y) * (self.max.z - self.min.z)
    }
    pub fn x_width(&self) -> f32 {
        self.max.x - self.min.x
    }
    pub fn y_height(&self) -> f32 {
        self.max.y - self.min.y
    }
    pub fn z_length(&self) -> f32 {
        self.max.z - self.min.z
    }
    pub fn size_on_axis(&self, axis: Axis) -> f32 {
        self.max[axis] - self.min[axis]
    }
    pub fn greatest_dimension(&self) -> Axis {
        ALL_AXES
            .into_iter()
            .max_by(|&axis1, &axis2| self.size_on_axis(axis1).partial_cmp(&self.size_on_axis(axis2)).unwrap())
            .unwrap()
    }
    pub fn expand_vec(&mut self, vec: Vec3d) {
        self.min.x = self.min.x.min(vec.x);
        self.min.y = self.min.y.min(vec.y);
        self.min.z = self.min.z.min(vec.z);
        self.max.x = self.max.x.max(vec.x);
        self.max.y = self.max.y.max(vec.y);
        self.max.z = self.max.z.max(vec.z);
    }
    pub fn expand_bbox(&mut self, bbox: &BoundingBox) {
        self.min.x = self.min.x.min(bbox.min.x);
        self.min.y = self.min.y.min(bbox.min.y);
        self.min.z = self.min.z.min(bbox.min.z);
        self.max.x = self.max.x.max(bbox.max.x);
        self.max.y = self.max.y.max(bbox.max.y);
        self.max.z = self.max.z.max(bbox.max.z);
    }
    pub fn from_vectors(mut iter: impl Iterator<Item = Vec3d>) -> BoundingBox {
        if let Some(vec) = iter.next() {
            iter.fold(BoundingBox { min: vec, max: vec }, |mut bbox, vec| {
                bbox.expand_vec(vec);
                bbox
            })
        } else {
            BoundingBox::default()
        }
    }
    pub fn from_bboxes<'a>(mut iter: impl Iterator<Item = &'a Self>) -> BoundingBox {
        if let Some(bbox) = iter.next() {
            iter.fold(*bbox, |mut acc_bbox, bbox| {
                acc_bbox.expand_bbox(bbox);
                acc_bbox
            })
        } else {
            BoundingBox::default()
        }
    }

    pub fn pad(mut self, pad: f32) -> BoundingBox {
        self.min.x -= pad;
        self.min.y -= pad;
        self.min.z -= pad;
        self.max.x += pad;
        self.max.y += pad;
        self.max.z += pad;
        self
    }

    pub fn contains(self, vec: Vec3d) -> bool {
        for axis in ALL_AXES {
            if vec[axis] < self.min[axis] || vec[axis] > self.max[axis] {
                return false;
            }
        }
        true
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
    // this is pretty much unused by the engine
    pub struct BspLight {
        pub location: Vec3d,
        pub kind: BspLightKind,
    }

    #[derive(Default, Debug, Clone)]
    pub struct EyePoint {
        pub attached_subobj: ObjectId,
        pub offset: Vec3d,
        pub normal: NormalVec3,
    }

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

#[derive(Clone)]
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
        bbox: BoundingBox,
        front: Box<ShieldNode>,
        back: Box<ShieldNode>,
    },
    Leaf {
        bbox: BoundingBox,
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

        crate::write::write_shield_node(&mut buf, self, get_version!() < Version::V21_18)?;

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

#[derive(Default, Debug, Clone)]
pub struct WeaponHardpoint {
    pub position: Vec3d,
    pub normal: NormalVec3,
    pub offset: f32,
}
impl Serialize for WeaponHardpoint {
    fn write_to(&self, w: &mut impl Write) -> io::Result<()> {
        self.position.write_to(w)?;
        self.normal.write_to(w)?;
        let version = get_version!();
        if version >= Version::V22_01 || version == Version::V21_18 {
            self.offset.write_to(w)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct ThrusterGlow {
    pub position: Vec3d,
    pub normal: Vec3d,
    pub radius: f32,
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
impl Serialize for ThrusterGlow {
    fn write_to(&self, w: &mut impl Write) -> io::Result<()> {
        self.position.write_to(w)?;
        self.normal.write_to(w)?;
        if get_version!() > Version::V20_04 {
            self.radius.write_to(w)?;
        }
        Ok(())
    }
}

mk_struct! {
    #[derive(Debug, Clone, Copy, PartialEq)]
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

#[derive(Debug, Default)]
pub struct ObjHeader {
    pub max_radius: f32,
    pub obj_flags: u32,
    pub num_subobjects: u32,
    pub bbox: BoundingBox,
    pub detail_levels: Vec<ObjectId>,
    pub mass: f32,
    pub center_of_mass: Vec3d,
    pub moment_of_inertia: Mat3d,
    pub cross_sections: Vec<(f32, f32)>, // depth, radius
    pub bsp_lights: Vec<BspLight>,
}

#[derive(Debug)]
pub struct ShieldData {
    pub verts: Vec<Vec3d>,
    pub polygons: Vec<ShieldPolygon>,
    pub collision_tree: Option<ShieldNode>,
}
impl ShieldData {
    pub fn recalculate_tree(verts: &[Vec3d], polygons: &[ShieldPolygon]) -> ShieldNode {
        // these structs make up the smallest bits of data we'll need for this
        // the regular data structure isn't well-optimized for this, so its easier to make something purpose built
        struct ShieldPolyInfo {
            id: PolygonId,
            bbox: BoundingBox,
            center: Vec3d,
        }

        // go over the polygons, making 'infos'
        let poly_infos = polygons
            .iter()
            .enumerate()
            .map(|(i, poly)| {
                // hacky way to turn a tuple into an iterator
                let verts = vec![
                    verts[poly.verts.0 .0 as usize],
                    verts[poly.verts.1 .0 as usize],
                    verts[poly.verts.2 .0 as usize],
                ];
                let vert_iter = verts.iter().copied();

                ShieldPolyInfo {
                    id: PolygonId(i as u32),
                    bbox: BoundingBox::from_vectors(vert_iter.clone()).pad(0.01),
                    center: Vec3d::average(vert_iter),
                }
            })
            .collect::<Vec<_>>();

        fn recalc_recurse(poly_infos: &mut [&ShieldPolyInfo]) -> ShieldNode {
            if let [poly_info] = *poly_infos {
                // if theres only one polygon we're at the base case
                ShieldNode::Leaf { bbox: poly_info.bbox, poly_list: vec![poly_info.id] }
            } else {
                let bbox = BoundingBox::from_bboxes(poly_infos.iter().map(|poly_info| &poly_info.bbox)).pad(0.01);
                let axis = bbox.greatest_dimension();
                poly_infos.sort_by(|a, b| a.center[axis].partial_cmp(&b.center[axis]).unwrap());

                let halfpoint = poly_infos.len() / 2;

                ShieldNode::Split {
                    front: Box::new(recalc_recurse(&mut poly_infos[..halfpoint])),
                    back: Box::new(recalc_recurse(&mut poly_infos[halfpoint..])),
                    bbox,
                }
            }
        }

        if poly_infos.is_empty() {
            ShieldNode::Leaf { bbox: BoundingBox::default(), poly_list: vec![] }
        } else {
            recalc_recurse(&mut poly_infos.iter().collect::<Vec<_>>())
        }
    }
}

#[derive(Clone, Debug)]
pub struct Polygon {
    pub normal: Vec3d,
    pub center: Vec3d,
    pub radius: f32,
    pub texture: Texturing,
    pub verts: Vec<PolyVertex>,
}

#[derive(Debug, Clone)]
pub enum BspNode {
    Split {
        normal: Vec3d,
        point: Vec3d,
        bbox: BoundingBox,
        front: Box<BspNode>,
        back: Box<BspNode>,
    },
    Leaf {
        bbox: BoundingBox,
        poly_list: Vec<Polygon>,
    },
}
impl Default for BspNode {
    fn default() -> Self {
        Self::Leaf { bbox: BoundingBox::default(), poly_list: vec![] }
    }
}
impl BspNode {
    pub fn leaves(&self) -> BspNodeIter<'_> {
        BspNodeIter { stack: vec![self] }
    }

    pub fn sum_of_bboxes(&self) -> f32 {
        match self {
            BspNode::Split { bbox, front, back, .. } => bbox.volume() + front.sum_of_bboxes() + back.sum_of_bboxes(),
            BspNode::Leaf { bbox, poly_list } => bbox.volume() * poly_list.len() as f32,
        }
    }

    pub fn sum_depth_and_size(&self) -> (u32, u32) {
        match self {
            BspNode::Split { front, back, .. } => {
                let (depth1, sz1) = front.sum_depth_and_size();
                let (depth2, sz2) = back.sum_depth_and_size();
                (depth1 + depth2 + sz1 + sz2, sz1 + sz2)
            }
            BspNode::Leaf { poly_list, .. } => (0, poly_list.len() as u32),
        }
    }

    pub fn recalculate_bboxes(&mut self, verts: &[Vec3d]) {
        match self {
            BspNode::Split { bbox, front, back, .. } => {
                front.recalculate_bboxes(verts);
                back.recalculate_bboxes(verts);
                bbox.min = {
                    let min1 = match **front {
                        BspNode::Split { ref bbox, .. } => &bbox.min,
                        BspNode::Leaf { ref bbox, .. } => &bbox.min,
                    };
                    let min2 = match **back {
                        BspNode::Split { ref bbox, .. } => &bbox.min,
                        BspNode::Leaf { ref bbox, .. } => &bbox.min,
                    };
                    Vec3d {
                        x: f32::min(min1.x, min2.x),
                        y: f32::min(min1.y, min2.y),
                        z: f32::min(min1.z, min2.z),
                    }
                };
                bbox.max = {
                    let max1 = match **front {
                        BspNode::Split { ref bbox, .. } => &bbox.max,
                        BspNode::Leaf { ref bbox, .. } => &bbox.max,
                    };
                    let max2 = match **back {
                        BspNode::Split { ref bbox, .. } => &bbox.max,
                        BspNode::Leaf { ref bbox, .. } => &bbox.max,
                    };
                    Vec3d {
                        x: f32::max(max1.x, max2.x),
                        y: f32::max(max1.y, max2.y),
                        z: f32::max(max1.z, max2.z),
                    }
                };
            }
            BspNode::Leaf { bbox, poly_list } => {
                *bbox = Default::default();
                for poly in poly_list {
                    for vert_id in &poly.verts {
                        bbox.min = {
                            Vec3d {
                                x: f32::min(bbox.min.x, verts[vert_id.vertex_id.0 as usize].x),
                                y: f32::min(bbox.min.y, verts[vert_id.vertex_id.0 as usize].y),
                                z: f32::min(bbox.min.z, verts[vert_id.vertex_id.0 as usize].z),
                            }
                        };
                        bbox.max = {
                            Vec3d {
                                x: f32::max(bbox.max.x, verts[vert_id.vertex_id.0 as usize].x),
                                y: f32::max(bbox.max.y, verts[vert_id.vertex_id.0 as usize].y),
                                z: f32::max(bbox.max.z, verts[vert_id.vertex_id.0 as usize].z),
                            }
                        };
                    }
                }
            }
        }
    }
}

pub struct BspNodeIter<'a> {
    stack: Vec<&'a BspNode>,
}

impl<'a> Iterator for BspNodeIter<'a> {
    type Item = (&'a BoundingBox, &'a Vec<Polygon>);

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

#[derive(Debug, Clone, Default)]
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
impl BspData {
    pub fn recalculate(verts: &[Vec3d], polygons: impl Iterator<Item = Polygon>) -> BspNode {
        // first go over the polygons, filling some data, and exporting their bboxes, which is important for the actual BSP generation
        let polygons = polygons
            .map(|mut poly| {
                let vert_iter = poly.verts.iter().map(|polyvert| verts[polyvert.vertex_id.0 as usize]);

                poly.center = Vec3d::average(vert_iter.clone());

                // generate the normal by averaging the cross products of adjacent edges
                let mut glm_verts = vert_iter.clone().map(Vec3::from); // first convert to glm vectors
                poly.normal = if poly.verts.len() == 3 {
                    // optimize a bit for for triangles, which we'll have a lot of
                    if let [Some(a), Some(b), Some(c)] = [glm_verts.next(), glm_verts.next(), glm_verts.next()] {
                        (a - b).cross(&(b - c)).into()
                    } else {
                        unreachable!()
                    }
                } else {
                    Vec3d::average(
                        glm_verts
                            .cycle() // cycle through the verts indefinitely...
                            .take(poly.verts.len() + 2) // ..but stop 2 after the end...
                            .collect::<Vec<_>>()
                            .windows(3) //...so with windows(3) we get 1,2,3 -> 2,3,1 -> 3,1,2 (for an example with 3 verts)
                            .map(|verts| (verts[0] - verts[1]).cross(&(verts[1] - verts[2])).into()), // ...and then get the cross from each triple, and average them
                    )
                }
                .normalize(); // and then normalize

                (BoundingBox::from_vectors(vert_iter).pad(0.01), poly)
            })
            .collect::<Vec<_>>();

        fn recalc_recurse(polygons: &mut [&(BoundingBox, Polygon)]) -> BspNode {
            if let [&(bbox, ref polygon)] = *polygons {
                // if theres only one polygon were at the base case
                BspNode::Leaf { bbox, poly_list: vec![polygon.clone()] }
            } else {
                let bbox = BoundingBox::from_bboxes(polygons.iter().map(|(bbox, _)| bbox)).pad(0.01);
                let axis = bbox.greatest_dimension();
                polygons.sort_by(|a, b| a.1.center[axis].partial_cmp(&b.1.center[axis]).unwrap());

                let halfpoint = polygons.len() / 2;

                BspNode::Split {
                    front: Box::new(recalc_recurse(&mut polygons[..halfpoint])),
                    back: Box::new(recalc_recurse(&mut polygons[halfpoint..])),
                    bbox,
                    normal: Vec3d::ZERO, // pretty sure these arent used...
                    point: Vec3d::ZERO,
                }
            }
        }

        if polygons.is_empty() {
            BspNode::Leaf { bbox: BoundingBox::default(), poly_list: vec![] }
        } else {
            recalc_recurse(&mut polygons.iter().collect::<Vec<_>>())
        }
    }
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
        if get_version!() >= Version::V21_17 {
            self.properties.write_to(w)?;
        }
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
    #[derive(Debug, Clone, Copy, PartialEq)]
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

pub const MAX_DEBRIS_OBJECTS: u32 = 32;

#[derive(Debug, Clone, Default)]
pub struct SubObject {
    pub obj_id: ObjectId,
    pub radius: f32,
    pub(crate) parent: Option<ObjectId>,
    pub offset: Vec3d,
    pub geo_center: Vec3d,
    pub bbox: BoundingBox,
    pub name: String,
    pub properties: String,
    pub movement_type: SubsysMovementType,
    pub movement_axis: SubsysMovementAxis,
    pub bsp_data: BspData,

    pub(crate) children: Vec<ObjectId>,
    pub is_debris_model: bool,
}
impl SubObject {
    pub fn parent(&self) -> Option<ObjectId> {
        self.parent
    }

    pub fn children(&self) -> std::slice::Iter<'_, ObjectId> {
        self.children.iter()
    }

    pub fn is_destroyed_model(&self) -> bool {
        self.name.to_lowercase().ends_with("-destroyed")
    }

    pub fn recalc_radius(&mut self) {
        self.radius = 0.00001;

        for vert in &self.bsp_data.verts {
            if vert.magnitude() > self.radius {
                self.radius = vert.magnitude();
            }
        }
    }

    pub fn recalc_bbox(&mut self) {
        self.bbox.min = self.bsp_data.verts[0];
        self.bbox.max = self.bsp_data.verts[0];

        for vert in &self.bsp_data.verts {
            if vert.x < self.bbox.min.x {
                self.bbox.min.x = vert.x
            }
            if vert.y < self.bbox.min.y {
                self.bbox.min.y = vert.y
            }
            if vert.z < self.bbox.min.z {
                self.bbox.min.z = vert.z
            }
            if vert.x > self.bbox.max.x {
                self.bbox.max.x = vert.x
            }
            if vert.y > self.bbox.max.y {
                self.bbox.max.y = vert.y
            }
            if vert.z > self.bbox.max.z {
                self.bbox.max.z = vert.z
            }
        }
        // self.bbox = match self.bsp_data.collision_tree {
        //     BspNode::Split { bbox, .. } => bbox,
        //     BspNode::Leaf { bbox, .. } => bbox,
        // };
    }
}
impl Serialize for SubObject {
    fn write_to(&self, w: &mut impl Write) -> io::Result<()> {
        let version = get_version!();
        self.obj_id.write_to(w)?;
        if version >= Version::V21_16 {
            self.radius.write_to(w)?;
            self.parent.unwrap_or(ObjectId(u32::MAX)).write_to(w)?;
            self.offset.write_to(w)?;
        } else {
            self.parent.unwrap_or(ObjectId(u32::MAX)).write_to(w)?;
            self.offset.write_to(w)?;
            self.radius.write_to(w)?;
        }
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
    pub position: Vec3d,
    pub fvec: NormalVec3,
    pub uvec: NormalVec3,
}
impl Serialize for Dock {
    fn write_to(&self, w: &mut impl Write) -> io::Result<()> {
        self.properties.write_to(w)?;
        match self.path {
            None => 0_u32.write_to(w)?,
            Some(x) => [x].write_to(w)?,
        }
        let points = vec![
            DockingPoint { position: self.position - self.uvec.0, normal: self.fvec.0 },
            DockingPoint { position: self.position + self.uvec.0, normal: self.fvec.0 },
        ];
        points.write_to(w)
    }
}
impl Dock {
    #[must_use]
    pub fn orthonormalize(&(mut xvec): &Vec3, fvec: &Vec3) -> NormalVec3 {
        xvec -= *fvec * xvec.dot(fvec);
        match xvec.try_into() {
            Ok(xvec) => xvec,
            Err(()) => {
                xvec = glm::vec3(0.0, 1.0, 0.0);
                (xvec - fvec * xvec.dot(fvec)).try_into().unwrap_or_default()
            }
        }
    }

    pub fn get_uvec_angle(&self) -> f32 {
        let mut fvec: Vec3 = self.fvec.0.into();
        if fvec.try_normalize_mut(1e-6).is_none() {
            fvec = glm::vec3(1.0, 0.0, 0.0);
        }
        let uvec = self.uvec.0.into();
        let xvec = glm::vec3(0.0, 0.0, 1.0);
        let xvec = Self::orthonormalize(&xvec, &fvec).0.into();
        f32::atan2(fvec.cross(&xvec).dot(&uvec), xvec.dot(&uvec))
    }

    pub fn set_uvec_angle(&mut self, ang: f32) {
        let mut fvec: Vec3 = self.fvec.0.into();
        if fvec.try_normalize_mut(1e-6).is_none() {
            fvec = glm::vec3(1.0, 0.0, 0.0);
        }
        let mut uvec = glm::vec3(0.0, 0.0, 1.0);
        let uvec = Self::orthonormalize(&mut uvec, &fvec).0.into();

        self.uvec = glm::rotate_vec3(&uvec, ang, &fvec).try_into().unwrap_or_default();
    }

    pub fn get_name(&self) -> Option<&str> {
        for str in self.properties.split('\n') {
            if let Some(name) = str.strip_prefix("$name=") {
                return Some(name);
            }
        }
        None
    }
}

mk_struct! {
    #[derive(Clone, Default)]
    pub struct Turret {
        pub base_obj: ObjectId,
        pub gun_obj: ObjectId,
        pub normal: NormalVec3,
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
impl GlowPointBank {
    pub fn get_glow_texture(&self) -> Option<&str> {
        self.properties.strip_prefix("$glow_texture=")
    }
    pub fn set_glow_texture(&mut self, tex: &str) {
        self.properties = format!("$glow_texture={}", tex);
    }
}

macro_rules! mk_versions {
    (@latest $last:ident) => { Self::$last };
    (@latest $first:ident $($rest:ident)*) => { mk_versions!(@latest $($rest)*) };
    ($($(#[doc=$doc:expr])* $name:ident($num:literal, $str:literal),)*) => {
        mk_enumeration! {
            #[derive(PartialOrd, Ord, PartialEq, Eq, Debug, Clone, Copy)]
            pub enum Version(i32) {
                $($(#[doc=$doc])* $name = $num,)*
            }
        }
        impl Version {
            pub const LATEST: Version = mk_versions!(@latest $($name)*);
            pub fn to_str(self) -> &'static str {
                match self {
                    $(Version::$name => $str,)*
                }
            }
            pub fn documentation(self) -> &'static str {
                match self {
                    $(Version::$name => concat!($($doc, "\n"),*).trim()),*
                }
            }
            pub fn for_each(mut f: impl FnMut(Self)) {
                $(f(Version::$name);)*
            }
        }
        impl Default for Version {
            fn default() -> Self {
                Self::LATEST
            }
        }
        impl Display for Version {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.to_str())
            }
        }
    };
}

mk_versions! {
    /// Prehistoric - Mass / MOI introduced
    V19_03(1903, "19.03"),
    /// Prehistoric - Glow point radius introduced after this
    V20_04(2004, "20.04"),
    /// Prehistoric - Muzzle flash introduced
    V20_07(2007, "20.07"),
    /// Prehistoric - Area mass conversion
    V20_09(2009, "20.09"),
    /// Retail FS1 - Cross sections introduced
    V20_14(2014, "20.14"),
    /// FS1 POFs produced by COB2FS v0.05
    V20_16(2016, "20.16"),
    /// Retail FS2 - PCS2 Compatible
    V21_16(2116, "21.16"),
    /// Retail FS2 - PCS2 Compatible - Thruster properties added
    V21_17(2117, "21.17"),
    /// External weapon angle offset added
    V21_18(2118, "21.18"),
    /// SLC2 replaces SLDC (no weapon offset compatibility)
    V22_00(2200, "22.00"),
    /// External weapon angle offset compatible
    V22_01(2201, "22.01"),
}

#[derive(Debug, Default)]
pub struct Model {
    pub version: Version,
    pub header: ObjHeader,
    pub sub_objects: ObjVec<SubObject>,
    pub textures: Vec<String>,
    pub paths: Vec<Path>,
    pub special_points: Vec<SpecialPoint>,
    pub eye_points: Vec<EyePoint>,
    pub primary_weps: Vec<Vec<WeaponHardpoint>>,
    pub secondary_weps: Vec<Vec<WeaponHardpoint>>,
    pub turrets: Vec<Turret>,
    pub thruster_banks: Vec<ThrusterBank>,
    pub glow_banks: Vec<GlowPointBank>,
    pub visual_center: Vec3d,
    pub comments: String,
    pub docking_bays: Vec<Dock>,
    pub insignias: Vec<Insignia>,
    pub shield_data: Option<ShieldData>,

    pub path_to_file: PathBuf,
}
impl Model {
    pub fn get_total_subobj_offset(&self, id: ObjectId) -> Vec3d {
        let mut subobj = &self.sub_objects[id];
        let mut out = subobj.offset;
        while let Some(parent) = subobj.parent {
            subobj = &self.sub_objects[parent];
            out += subobj.offset;
        }
        out
    }

    // see if maybe_ancestor is actually an ancestor of obj_id in the subobject hierarchy
    pub fn is_obj_id_ancestor(&self, obj_id: ObjectId, maybe_ancestor: ObjectId) -> bool {
        if obj_id == maybe_ancestor {
            return true;
        }

        let mut sub_obj_parent = self.sub_objects[obj_id].parent;
        loop {
            if sub_obj_parent == Some(maybe_ancestor) {
                return true;
            } else if sub_obj_parent.is_none() {
                return false;
            }
            sub_obj_parent = self.sub_objects[sub_obj_parent.unwrap()].parent;
        }
    }

    pub fn get_detail_level(&self, obj_id: ObjectId) -> Option<u32> {
        for (i, id) in self.header.detail_levels.iter().enumerate() {
            if self.is_obj_id_ancestor(obj_id, *id) {
                return Some(i as u32);
            }
        }
        None
    }

    pub fn get_subobj_names(&self) -> Vec<String> {
        let mut ret = vec![];
        for subobj in &self.sub_objects {
            ret.push(subobj.name.clone());
        }
        ret
    }

    pub fn get_obj_id_by_name(&self, name: &str) -> Option<ObjectId> {
        for subobj in &self.sub_objects {
            if subobj.name == name {
                return Some(subobj.obj_id);
            }
        }
        None
    }

    pub fn path_removal_fixup(&mut self, removed_idx: PathId) {
        for bay in &mut self.docking_bays {
            if let Some(path_num) = bay.path {
                if removed_idx == path_num {
                    bay.path = None;
                } else if path_num > removed_idx {
                    bay.path = Some(PathId(path_num.0 - 1));
                }
            }
        }
    }

    pub fn get_valid_gun_subobjects_for_turret(&self, existing_obj: ObjectId, turret_obj: ObjectId) -> (Vec<ObjectId>, usize) {
        let mut out_vec = vec![];
        let mut out_idx = 0;
        let mut found_existing_obj = false;

        // check the turret base object itself first
        if existing_obj == turret_obj {
            out_idx = out_vec.len();
            found_existing_obj = true;
        }
        out_vec.push(turret_obj);

        // then iterate through immediate base object children, which are also valid
        for &child_id in &self.sub_objects[turret_obj].children {
            if existing_obj == child_id {
                out_idx = out_vec.len();
                found_existing_obj = true;
            }
            out_vec.push(child_id);
        }

        // if none of the above are the current selection, that means its invalid!
        // invalidity is handled separately, but we have to add this invalid object either way, so append it at the end
        if !found_existing_obj {
            out_idx = out_vec.len();
            out_vec.push(existing_obj);
        }

        (out_vec, out_idx)
    }

    pub fn num_debris_objects(&self) -> u32 {
        let mut num_debris = 0;
        for sobj in &self.sub_objects {
            if sobj.is_debris_model {
                num_debris += 1;
            }
        }
        num_debris
    }

    pub fn apply_transform(&mut self, id: ObjectId, matrix: &TMat4<f32>, transform_offset: bool) {
        let zero = Vec3d::ZERO.into();
        let translation = matrix.transform_point(&zero) - zero;
        let matrix = &matrix.append_translation(&(-translation));

        let subobj = &mut self.sub_objects[id];
        subobj.radius = 0.0;
        for vert in &mut subobj.bsp_data.verts {
            *vert = matrix * *vert;
            if vert.magnitude() > subobj.radius {
                subobj.radius = vert.magnitude();
            }
        }

        // this preserves rotations, but inverts scales, which is the proper transformation for normals
        let norm_matrix = matrix.try_inverse().unwrap().transpose();

        for norm in &mut subobj.bsp_data.norms {
            *norm = (&norm_matrix * *norm).normalize();
        }

        subobj.bsp_data.collision_tree.recalculate_bboxes(&subobj.bsp_data.verts);

        subobj.bbox = {
            match subobj.bsp_data.collision_tree {
                BspNode::Split { bbox, .. } => bbox,
                BspNode::Leaf { bbox, .. } => bbox,
            }
        };

        if transform_offset {
            subobj.offset = matrix * subobj.offset;
        }

        let children = subobj.children.clone();

        for child_id in children {
            self.apply_transform(child_id, matrix, true)
        }
    }

    pub fn recalc_radius(&mut self) {
        self.header.max_radius = 0.00001;
        if let Some(&detail_0) = self.header.detail_levels.first() {
            for subobj in &self.sub_objects {
                if !self.is_obj_id_ancestor(subobj.obj_id, detail_0) {
                    continue;
                }

                let offset = self.get_total_subobj_offset(subobj.obj_id);
                for vert in &subobj.bsp_data.verts {
                    if (*vert + offset).magnitude() > self.header.max_radius {
                        self.header.max_radius = (*vert + offset).magnitude();
                    }
                }
            }
        }
    }

    pub fn recalc_bbox(&mut self) {
        let mut new_bbox = self.header.bbox;
        new_bbox.min = Vec3d { x: -0.00001, y: -0.00001, z: -0.00001 };
        new_bbox.max = Vec3d { x: 0.00001, y: 0.00001, z: 0.00001 };

        if let Some(&detail_0) = self.header.detail_levels.first() {
            for subobj in &self.sub_objects {
                if !self.is_obj_id_ancestor(subobj.obj_id, detail_0) {
                    continue;
                }

                let offset = self.get_total_subobj_offset(subobj.obj_id);
                let min = offset + subobj.bbox.min;
                let max = offset + subobj.bbox.max;
                new_bbox.min = Vec3d {
                    x: f32::min(new_bbox.min.x, min.x),
                    y: f32::min(new_bbox.min.y, min.y),
                    z: f32::min(new_bbox.min.z, min.z),
                };

                new_bbox.max = Vec3d {
                    x: f32::max(new_bbox.max.x, max.x),
                    y: f32::max(new_bbox.max.y, max.y),
                    z: f32::max(new_bbox.max.z, max.z),
                };
            }
        }

        self.header.bbox = new_bbox;
    }

    pub fn recalc_mass(&mut self) {
        self.header.mass = 4.65 * (self.header.bbox.volume().powf(2.0 / 3.0));
    }

    pub fn recalc_moi(&mut self) {
        self.header.moment_of_inertia = Mat3d::default();

        fn sum_verts_recurse(subobjects: &ObjVec<SubObject>, id: ObjectId) -> usize {
            subobjects[id].bsp_data.verts.len() + subobjects[id].children.iter().map(|id| sum_verts_recurse(subobjects, *id)).sum::<usize>()
        }

        if let Some(&detail_0) = self.header.detail_levels.first() {
            let num_verts = sum_verts_recurse(&self.sub_objects, detail_0);

            let point_mass = self.header.mass / num_verts as f32;

            fn accumulate_moi_recurse(subobjects: &ObjVec<SubObject>, id: ObjectId, moi: &mut Mat3d) {
                subobjects[id].bsp_data.verts.iter().for_each(|vert| moi.add_point_mass_moi(*vert));
                subobjects[id].children.iter().for_each(|id| accumulate_moi_recurse(subobjects, *id, moi));
            }

            accumulate_moi_recurse(&self.sub_objects, detail_0, &mut self.header.moment_of_inertia);

            let mut glm_mat: Mat3x3 = self.header.moment_of_inertia.into();
            glm_mat *= point_mass;
            self.header.moment_of_inertia = glm_mat.try_inverse().unwrap().into();
        }
    }

    pub fn clean_up(&mut self) {
        if let Some(shield) = &mut self.shield_data {
            if shield.collision_tree.is_none() {
                shield.collision_tree = Some(ShieldData::recalculate_tree(&shield.verts, &shield.polygons));
            }
        }
    }

    pub fn make_orphan(&mut self, would_be_orphan: ObjectId) {
        if let Some(parent_id) = self.sub_objects[would_be_orphan].parent {
            let parent_children = &mut self.sub_objects[parent_id].children;
            parent_children.remove(parent_children.iter().position(|child_id| *child_id == would_be_orphan).unwrap());
        }
        self.sub_objects[would_be_orphan].parent = None;
    }

    pub fn make_parent(&mut self, new_parent: ObjectId, new_child: ObjectId) -> Option<()> {
        if !self.is_obj_id_ancestor(new_parent, new_child) {
            self.sub_objects[new_parent].children.push(new_child);
            self.sub_objects[new_child].parent = Some(new_parent);
            Some(())
        } else {
            None
        }
    }
}
