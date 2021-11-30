use std::io::{self, Write};

use byteorder::{WriteBytesExt, LE};
use dae_parser::{Document, Library, LibraryElement};

use crate::{BspData, BspNode, Model, ObjVec, ShieldNode, SubObject, Texturing, Vec3d, Version};

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

fn align_buf(buf: &mut Vec<u8>) -> io::Result<()> {
    let padding_length = buf.len().wrapping_neg() % 4;
    buf.write_all(&[0; 4][..padding_length])
}

pub(crate) fn write_shield_node(buf: &mut Vec<u8>, shield_node: &ShieldNode) -> io::Result<()> {
    match shield_node {
        ShieldNode::Split { bbox, front, back } => {
            let base = buf.len();
            buf.write_u32::<LE>(ShieldNode::SPLIT)?;
            let chunk_size_pointer = Fixup::new(buf, base)?;

            bbox.write_to(buf)?;
            let front_offset = Fixup::new(buf, base)?;
            let back_offset = Fixup::new(buf, base)?;

            front_offset.finish(buf);
            write_shield_node(buf, front)?;

            back_offset.finish(buf);
            write_shield_node(buf, back)?;

            chunk_size_pointer.finish(buf);
        }
        ShieldNode::Leaf { bbox, poly_list } => {
            let base = buf.len();
            buf.write_u32::<LE>(ShieldNode::LEAF)?;
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
    fn finish(self, buf: &mut Vec<u8>) {
        let size = (buf.len() - self.base) as u32;
        buf[self.location..][..4].copy_from_slice(&u32::to_le_bytes(size))
    }
}

pub(crate) fn write_bsp_data(buf: &mut Vec<u8>, bsp_data: &BspData) -> io::Result<()> {
    const MAX_NORMS_PER_VERT: u8 = 0xCC; //u8::MAX;

    fn write_bsp_node(buf: &mut Vec<u8>, bsp_node: &BspNode) -> io::Result<()> {
        match bsp_node {
            BspNode::Split { normal, point, bbox, front, back } => {
                let base = buf.len();
                buf.write_u32::<LE>(BspData::SORTNORM)?;
                let chunk_size_pointer = Fixup::new(buf, base)?;

                normal.write_to(buf)?;
                point.write_to(buf)?;

                buf.write_u32::<LE>(0)?;

                let front_offset = Fixup::new(buf, base)?;
                let back_offset = Fixup::new(buf, base)?;

                buf.write_u32::<LE>(0)?;
                buf.write_u32::<LE>(0)?;
                buf.write_u32::<LE>(0)?;
                bbox.write_to(buf)?;

                front_offset.finish(buf);
                write_bsp_node(buf, front)?;

                back_offset.finish(buf);
                write_bsp_node(buf, back)?;

                chunk_size_pointer.finish(buf);
            }
            BspNode::Leaf { bbox, poly_list } => {
                let base = buf.len();
                buf.write_u32::<LE>(BspData::BOUNDBOX)?;
                let chunk_size_pointer = Fixup::new(buf, base)?;

                bbox.write_to(buf)?;
                chunk_size_pointer.finish(buf);

                for poly in poly_list {
                    let base = buf.len();
                    match &poly.texture {
                        Texturing::Flat(_) => buf.write_u32::<LE>(BspData::FLATPOLY)?,
                        Texturing::Texture(_) => buf.write_u32::<LE>(BspData::TMAPPOLY)?,
                    }
                    let chunk_size_pointer = Fixup::new(buf, base)?;

                    poly.normal.write_to(buf)?;
                    poly.center.write_to(buf)?;
                    poly.radius.write_to(buf)?;
                    (poly.verts.len() as u32).write_to(buf)?;
                    poly.texture.write_to(buf)?;

                    for vert in &poly.verts {
                        vert.vertex_id.write_to(buf)?;
                        vert.normal_id.write_to(buf)?;
                        if matches!(poly.texture, Texturing::Texture(_)) {
                            vert.uv.write_to(buf)?;
                        }
                    }

                    chunk_size_pointer.finish(buf);
                }

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

    write_bsp_node(buf, &bsp_data.collision_tree)?;

    Ok(())
}

fn write_chunk_raw(w: &mut impl Write, chunk_name: &[u8], f: impl FnOnce(&mut Vec<u8>) -> io::Result<()>) -> io::Result<()> {
    w.write_all(chunk_name)?;
    //println!("writing chunk {}", std::str::from_utf8(chunk_name).unwrap());

    let mut buf = vec![];

    f(&mut buf)?;

    w.write_u32::<LE>((buf.len()) as u32)?;
    //println!("writing chunk length {}", buf.len());
    w.write_all(&buf)
}

fn write_chunk<T: Serialize>(w: &mut impl Write, chunk_name: &[u8], data: Option<&T>) -> io::Result<()> {
    if let Some(data) = data {
        write_chunk_raw(w, chunk_name, |w| data.write_to(w))?
    }
    Ok(())
}

fn write_chunk_vec<T: Serialize>(w: &mut impl Write, chunk_name: &[u8], data: &Vec<T>) -> io::Result<()> {
    if !data.is_empty() {
        write_chunk_raw(w, chunk_name, |w| data.write_to(w))?
    }
    Ok(())
}

impl Model {
    pub fn write(&self, w: &mut impl Write) -> io::Result<()> {
        w.write_all(b"PSPO")?;

        w.write_i32::<LE>(Version::LATEST.into())?;

        write_chunk_raw(w, b"HDR2", |w| {
            self.header.max_radius.write_to(w)?;
            self.header.obj_flags.write_to(w)?;
            self.header.num_subobjects.write_to(w)?;
            self.header.bounding_box.write_to(w)?;
            self.header.detail_levels.write_to(w)?;
            self.sub_objects
                .iter()
                .filter(|obj| obj.is_debris_model)
                .map(|obj| obj.obj_id)
                .collect::<Vec<_>>()
                .write_to(w)?;
            self.header.mass.write_to(w)?;
            self.header.center_of_mass.write_to(w)?;
            self.header.moment_of_inertia.write_to(w)?;
            self.header.cross_sections.write_to(w)?;
            self.header.bsp_lights.write_to(w)
        })?;
        for subobject in &self.sub_objects {
            write_chunk(w, b"OBJ2", Some(subobject))?;
        }
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

            write_chunk(w, b"SLC2", shield_data.collision_tree.as_ref())?;
        }
        if self.auto_center != Vec3d::default() {
            write_chunk(w, b"ACEN", Some(&self.auto_center))?;
        }

        Ok(())
    }
}
use dae_parser::*;
fn make_subobj_node(subobjs: &ObjVec<SubObject>, subobj: &SubObject, geometries: &mut Vec<Geometry>, materials: &[String]) -> Node {
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
        positions.push(vert.x);
        positions.push(vert.z);
        positions.push(vert.y);
    }

    let mut normals = vec![];
    for norm in &subobj.bsp_data.norms {
        normals.push(norm.x);
        normals.push(norm.z);
        normals.push(norm.y);
    }

    let mut uv_coords = vec![];
    let mut uv_len = 0;
    let mut prim_elems = vec![(vec![], vec![]); materials.len() + 1];

    for (_, polylist) in subobj.bsp_data.collision_tree.leaves() {
        for poly in polylist {
            let (vert_count, indices) = &mut prim_elems[match poly.texture {
                Texturing::Flat(_) => 0,
                Texturing::Texture(idx) => idx.0 as usize + 1,
            }];
            vert_count.push(poly.verts.len() as u32);
            for vert in poly.verts.iter().rev() {
                indices.push(vert.vertex_id.0 as _);
                indices.push(vert.normal_id.0 as _);
                indices.push(uv_len);
                uv_len += 1;
                uv_coords.push(vert.uv.0);
                uv_coords.push(vert.uv.1);
            }
        }
    }

    let mut instance = Instance::<Geometry>::new(Url::Fragment(geo_id.clone()));
    let bind_materials = prim_elems[1..]
        .iter()
        .zip(materials)
        .filter(|((vcount, _), _)| !vcount.is_empty())
        .map(|(_, mat)| {
            InstanceMaterial::new(
                mat.to_string(),
                Url::Fragment(format!("{}-material", mat)),
                vec![BindVertexInput::new("UVMap", "TEXCOORD", Some(0))],
            )
        })
        .collect::<Vec<_>>();
    if !bind_materials.is_empty() {
        instance.data.bind_material = Some(BindMaterial::new(bind_materials));
    }

    geometries.push(Geometry::new_mesh(
        geo_id.clone(),
        vec![
            Source::new_local(
                pos_id.clone(),
                Param::new_xyz(),
                ArrayElement::Float(FloatArray { id: Some(pos_array_id.clone()), val: positions.into() }), // TODO make a new func
            ),
            Source::new_local(
                norm_id.clone(),
                Param::new_xyz(),
                ArrayElement::Float(FloatArray { id: Some(norm_array_id.clone()), val: normals.into() }),
            ),
            Source::new_local(
                uv_id.clone(),
                Param::new_st(),
                ArrayElement::Float(FloatArray { id: Some(uv_array_id.clone()), val: uv_coords.into() }),
            ),
        ],
        Vertices::new(vert_id.clone(), vec![Input::new(Semantic::Position, Url::Fragment(pos_id.clone()))]),
        prim_elems
            .into_iter()
            .zip([None].into_iter().chain(materials.iter().map(Some)))
            .filter(|((vcount, _), _)| !vcount.is_empty())
            .map(|((vcount, indices), material)| {
                // TODO
                Primitive::PolyList(PolyList::new(
                    material.cloned(),
                    vec![
                        InputS::new(Semantic::Vertex, Url::Fragment(vert_id.clone()), 0, None),
                        InputS::new(Semantic::Normal, Url::Fragment(norm_id.clone()), 1, None),
                        InputS::new(Semantic::TexCoord, Url::Fragment(uv_id.clone()), 2, None),
                    ],
                    vcount.into_boxed_slice(),
                    indices.into_boxed_slice(),
                ))
            })
            .collect(),
    ));

    let mut node = Node::new(subobj.name.clone(), Some(subobj.name.clone()));
    node.push_transform(Translate::new([subobj.offset.x, subobj.offset.z, subobj.offset.y]));

    node.instance_geometry.push(instance);
    node.children = subobj
        .children
        .iter()
        .map(|&id| make_subobj_node(subobjs, &subobjs[id], geometries, materials))
        .collect();

    node
}
impl Model {
    pub fn write_dae(&self, w: &mut impl Write) -> Result<(), dae_parser::Error> {
        let mut geometries = vec![];

        let mut nodes = vec![];

        for subobj in &self.sub_objects {
            if subobj.parent.is_none() {
                nodes.push(make_subobj_node(&self.sub_objects, subobj, &mut geometries, &self.textures));
            }
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

        doc.asset.up_axis = UpAxis::ZUp;

        doc.write_to(w)
    }
}
