use std::io::{self, Write};

use byteorder::{WriteBytesExt, LE};

use crate::{BspData, BspNode, Model, ShieldNode, Texturing, Vec3d, Version};

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
