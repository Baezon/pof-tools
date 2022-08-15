use crate::*;
use byteorder::{ReadBytesExt, LE};
use core::panic;
use dae_parser as dae;
use glm::Mat4x4;
use nalgebra_glm as glm;
use std::collections::HashMap;
use std::convert::TryInto;
use std::fs::File;
use std::io::{self, BufReader, ErrorKind, Read, Seek, SeekFrom};
use std::path::PathBuf;

pub struct Parser<R> {
    file: R,
    version: Version,
}
impl<R: Read + Seek> Parser<R> {
    pub fn new(mut file: R) -> io::Result<Parser<R>> {
        assert!(&read_bytes(&mut file)? == b"PSPO", "Not a freespace 2 pof file!");

        let version: Version = read_i32(&mut file)?.try_into().expect("Unrecognized pof version");

        // println!("The verison is {:?}", version);

        Ok(Parser { file, version })
    }

    pub fn parse(&mut self, path: PathBuf) -> io::Result<Model> {
        // println!("parsing new model!");
        let mut header = None;
        let mut sub_objects = vec![];
        let mut textures = None;
        let mut paths = None;
        let mut special_points = None;
        let mut eye_points = None;
        let mut primary_weps = None;
        let mut secondary_weps = None;
        let mut turrets = vec![];
        let mut thruster_banks = None;
        let mut comments = None;
        let mut dock_points = None;
        let mut glow_banks = None;
        let mut insignias = None;
        let mut visual_center = None;
        let mut shield_data = None;

        let mut shield_tree_chunk = None;
        let mut debris_objs = vec![];

        loop {
            let id = &match self.read_bytes() {
                Err(e) if e.kind() == ErrorKind::UnexpectedEof => break,
                id_result => id_result?,
            };
            let len = self.read_i32()?;

            // println!("found chunk {}", std::str::from_utf8(id).unwrap());
            // println!("length is {} bytes", len);
            match id {
                b"OHDR" | b"HDR2" => {
                    assert!(header.is_none());
                    assert_eq!(self.version >= Version::V21_16, id == b"HDR2");

                    let (max_radius, obj_flags, num_subobjects);
                    if self.version >= Version::V21_16 {
                        max_radius = self.read_f32()?;
                        obj_flags = self.read_u32()?;
                        num_subobjects = self.read_u32()?;
                    } else {
                        num_subobjects = self.read_u32()?;
                        max_radius = self.read_f32()?;
                        obj_flags = self.read_u32()?;
                    }

                    sub_objects = vec![None; num_subobjects as usize];

                    let bounding_box = self.read_bbox()?;

                    let detail_levels = self.read_list(|this| Ok(ObjectId(this.read_u32()?)))?;
                    debris_objs = self.read_list(|this| Ok(ObjectId(this.read_u32()?)))?;

                    let (mut mass, center_of_mass, mut moment_of_inertia);
                    if self.version >= Version::V19_03 {
                        mass = self.read_f32()?;
                        center_of_mass = self.read_vec3d()?;
                        moment_of_inertia = Mat3d {
                            rvec: self.read_vec3d()?,
                            uvec: self.read_vec3d()?,
                            fvec: self.read_vec3d()?,
                        };
                        if self.version < Version::V20_09 {
                            // migration code ported from FSO
                            let area_mass = mass.powf(0.6667) * 4.65;
                            moment_of_inertia *= mass / area_mass;
                            mass = area_mass;
                        }
                    } else {
                        mass = 50.0; // default used by FSO
                        center_of_mass = Vec3d::ZERO;
                        moment_of_inertia = Mat3d::IDENTITY;
                        moment_of_inertia *= 0.001;
                    };

                    let cross_sections = if self.version >= Version::V20_14 {
                        let num_cross_sections = match self.read_u32()? {
                            u32::MAX => 0,
                            n => n,
                        };
                        self.read_list_n(num_cross_sections as usize, |this| Ok((this.read_f32()?, this.read_f32()?)))?
                    } else {
                        vec![]
                    };

                    let bsp_lights = if self.version >= Version::V20_07 {
                        self.read_list(|this| {
                            Ok(BspLight {
                                location: this.read_vec3d()?,
                                kind: match this.read_u32()? {
                                    1 => BspLightKind::Muzzle,
                                    2 => BspLightKind::Thruster,
                                    _ => panic!(), // maybe dont just panic
                                },
                            })
                        })?
                    } else {
                        vec![]
                    };

                    header = Some(ObjHeader {
                        num_subobjects,
                        max_radius,
                        obj_flags,
                        bbox: bounding_box,
                        detail_levels,
                        mass,
                        center_of_mass,
                        moment_of_inertia,
                        cross_sections,
                        bsp_lights,
                    });
                    //println!("{:#?}", header)
                }
                b"SOBJ" | b"OBJ2" => {
                    assert!(header.is_some());
                    assert_eq!(self.version >= Version::V21_16, id == b"OBJ2");

                    let obj_id = ObjectId(self.read_u32()?); //id

                    let (radius, parent, offset);
                    if self.version >= Version::V21_16 {
                        radius = self.read_f32()?;
                        parent = self.read_u32()?;
                        offset = self.read_vec3d()?;
                    } else {
                        parent = self.read_u32()?;
                        offset = self.read_vec3d()?;
                        radius = self.read_f32()?;
                    }
                    let parent = if parent == u32::MAX {
                        None
                    } else {
                        assert!(sub_objects[parent as usize].is_some(), "parent out of order");
                        Some(ObjectId(parent))
                    };

                    let geo_center = self.read_vec3d()?;
                    let bbox = self.read_bbox()?;
                    let name = self.read_string()?;
                    let properties = self.read_string()?;
                    let movement_type = self.read_i32()?.try_into().unwrap_or_default();
                    let mut movement_axis = self.read_i32()?.try_into().unwrap_or_default();
                    if movement_type == SubsysMovementType::None {
                        movement_axis = SubsysMovementAxis::None
                    }

                    assert!(self.read_i32()? == 0, "chunked models unimplemented in FSO");
                    let bsp_data_buffer = self.read_byte_buffer()?;
                    let bsp_data = parse_bsp_data(&bsp_data_buffer, self.version)?;
                    //println!("parsed subobject {}", name);

                    assert!(sub_objects[obj_id.0 as usize].is_none());
                    sub_objects[obj_id.0 as usize] = Some(SubObject {
                        obj_id,
                        radius,
                        parent,
                        offset,
                        geo_center,
                        bbox,
                        name,
                        properties,
                        movement_type,
                        movement_axis,
                        bsp_data,
                        // these two are to be filled later once we've parsed all the subobjects
                        children: vec![],
                        is_debris_model: false,
                    });
                    //println!("parsed subobject {:#?}", sub_objects[obj_id.0 as usize]);
                }
                b"TXTR" => {
                    assert!(textures.is_none());

                    textures = Some(self.read_list(|this| this.read_string())?);
                    //println!("{:#?}", textures);
                }
                b"PATH" => {
                    assert!(paths.is_none());

                    paths = Some(self.read_list(|this| {
                        Ok(Path {
                            name: this.read_string()?,
                            parent: if this.version >= Version::V20_02 {
                                this.read_string()?
                            } else {
                                String::new()
                            },
                            points: this.read_list(|this| {
                                Ok(PathPoint {
                                    position: this.read_vec3d()?,
                                    radius: this.read_f32()?,
                                    turrets: this.read_list(|this| Ok(ObjectId(this.read_u32()?)))?,
                                })
                            })?,
                        })
                    })?);
                    //println!("{:#?}", paths);
                }
                b"SPCL" => {
                    assert!(special_points.is_none());

                    special_points = Some(self.read_list(|this| {
                        Ok(SpecialPoint {
                            name: this.read_string()?,
                            properties: this.read_string()?,
                            position: this.read_vec3d()?,
                            radius: this.read_f32()?,
                        })
                    })?);
                    //println!("{:#?}", special_points);
                }
                b"EYE " => {
                    eye_points = Some(self.read_list(|this| {
                        Ok(EyePoint {
                            attached_subobj: ObjectId(this.read_u32()?),
                            offset: this.read_vec3d()?,
                            normal: this.read_vec3d()?.try_into().unwrap_or_default(),
                        })
                    })?);
                    //println!("{:#?}", eye_points);
                }
                b"GPNT" | b"MPNT" => {
                    let target = if id == b"GPNT" { &mut primary_weps } else { &mut secondary_weps };
                    assert!(target.is_none());
                    *target = Some(self.read_list(|this| {
                        this.read_list(|this| {
                            Ok(WeaponHardpoint {
                                position: this.read_vec3d()?,
                                normal: this.read_vec3d()?.try_into().unwrap_or_default(),
                                // TODO: document this at https://wiki.hard-light.net/index.php/POF_data_structure
                                offset: if this.version >= Version::V21_18 && this.version != Version::V22_00 {
                                    this.read_f32()?
                                } else {
                                    0.0
                                },
                            })
                        })
                    })?);
                    //println!("{:#?}", target);
                }
                b"TGUN" | b"TMIS" => {
                    turrets.extend(self.read_list(|this| {
                        let base_obj = ObjectId(this.read_u32()?);
                        let gun_obj = ObjectId(this.read_u32()?);
                        assert!(sub_objects[base_obj.0 as usize].is_some(), "turret precedes base object");
                        assert!(sub_objects[gun_obj.0 as usize].is_some(), "turret precedes gun object");
                        Ok(Turret {
                            base_obj,
                            gun_obj,
                            normal: this.read_vec3d()?.try_into().unwrap_or_default(),
                            fire_points: this.read_list(|this| this.read_vec3d())?,
                        })
                    })?);
                    //println!("{:#?}", turrets);
                }
                b"FUEL" => {
                    assert!(thruster_banks.is_none());
                    thruster_banks = Some(self.read_list(|this| {
                        let num_glows = this.read_u32()?;
                        Ok(ThrusterBank {
                            properties: if this.version >= Version::V21_17 {
                                this.read_string()?
                            } else {
                                String::new()
                            },
                            glows: this.read_list_n(num_glows as usize, |this| {
                                Ok(ThrusterGlow {
                                    position: this.read_vec3d()?,
                                    normal: this.read_vec3d()?,
                                    // TODO document this at https://wiki.hard-light.net/index.php/POF_data_structure
                                    radius: if this.version > Version::V20_04 { this.read_f32()? } else { 1.0 },
                                })
                            })?,
                        })
                    })?);
                    //println!("{:#?}", thruster_banks);
                }
                b"GLOW" => {
                    assert!(glow_banks.is_none());
                    glow_banks = Some(self.read_list(|this| {
                        let num_glow_points;
                        Ok(GlowPointBank {
                            disp_time: this.read_i32()?,
                            on_time: this.read_u32()?,
                            off_time: this.read_u32()?,
                            obj_parent: ObjectId(this.read_u32()?),
                            lod: this.read_u32()?,
                            glow_type: this.read_u32()?,
                            properties: {
                                num_glow_points = this.read_u32()?;
                                this.read_string()?
                            },
                            glow_points: this.read_list_n(num_glow_points as usize, |this| {
                                Ok(GlowPoint {
                                    position: this.read_vec3d()?,
                                    normal: this.read_vec3d()?,
                                    radius: this.read_f32()?,
                                })
                            })?,
                        })
                    })?);
                    //println!("{:#?}", glow_banks);
                }
                b"ACEN" => {
                    assert!(visual_center.is_none());
                    visual_center = Some(self.read_vec3d()?);
                }
                b"DOCK" => {
                    assert!(dock_points.is_none());
                    dock_points = Some(self.read_list(|this| {
                        let properties = this.read_string()?;
                        let paths = this.read_list(|this| this.read_u32())?; // spec allows for a list of paths but only the first will be used so dont bother
                        let path = paths.first().map(|&x| PathId(x));
                        // same thing here, only first 2 are used
                        let mut dockpoints = this.read_list(|this| Ok(DockingPoint { position: this.read_vec3d()?, normal: this.read_vec3d()? }))?;
                        let mut iter = dockpoints.drain(..2);
                        let (p1, p2) = (iter.next().unwrap_or_default(), iter.next().unwrap_or_default());
                        let position = (p1.position + p2.position) / 2.0;
                        let fvec: NormalVec3 = p1.normal.try_into().unwrap_or_default();
                        let uvec = Dock::orthonormalize(&(p2.position - p1.position).into(), &fvec.0.into());

                        Ok(Dock { properties, path, position, fvec, uvec: uvec.into() })
                    })?);
                    //println!("{:#?}", dock_points);
                }
                b"INSG" => {
                    assert!(insignias.is_none());
                    insignias = Some(self.read_list(|this| {
                        let num_faces;
                        Ok(Insignia {
                            detail_level: this.read_u32()?,
                            vertices: {
                                num_faces = this.read_u32()?;
                                this.read_list(|this| this.read_vec3d())?
                            },
                            offset: this.read_vec3d()?,
                            faces: this.read_list_n(num_faces as usize, |this| {
                                let [x, y, z] = *this.read_array(|this| {
                                    Ok(PolyVertex {
                                        vertex_id: VertexId(this.read_u32()?),
                                        normal_id: (),
                                        uv: (this.read_f32()?, this.read_f32()?),
                                    })
                                })?;
                                Ok((x, y, z))
                            })?,
                        })
                    })?);
                    //println!("{:#?}", insignias);
                }
                b"SHLD" => {
                    assert!(shield_data.is_none());
                    shield_data = Some((
                        self.read_list(|this| this.read_vec3d())?,
                        self.read_list(|this| {
                            Ok(ShieldPolygon {
                                normal: this.read_vec3d()?,
                                verts: (VertexId(this.read_u32()?), VertexId(this.read_u32()?), VertexId(this.read_u32()?)),
                                neighbors: (PolygonId(this.read_u32()?), PolygonId(this.read_u32()?), PolygonId(this.read_u32()?)),
                            })
                        })?,
                    ))
                }
                b"SLDC" | b"SLC2" => {
                    assert!(shield_tree_chunk.is_none());
                    assert_eq!(self.version >= Version::V22_00, id == b"SLC2");
                    // deal with this later, once we're sure to also have the shield data
                    shield_tree_chunk = Some(self.read_byte_buffer()?);
                }
                b"PINF" => {
                    assert!(comments.is_none());
                    // gotta inline some stuff because the length of this string is the length of the chunk
                    let mut buffer = vec![0; len as usize];
                    self.file.read_exact(&mut buffer)?;

                    let end = buffer.iter().position(|&char| char == 0).unwrap_or(buffer.len());
                    comments = Some(String::from_utf8(buffer[..end].into()).unwrap());
                    // println!("{:#?}", comments);
                }
                _ => {
                    eprintln!("I don't know how to handle id {:x?}", id);
                    self.file.seek(SeekFrom::Current(len as i64))?;
                }
            }
        }

        // finally handle the shield tree, if applicable
        let shield_data = match (shield_data, shield_tree_chunk) {
            (Some((verts, poly_list)), shield_tree_chunk) => Some(ShieldData {
                verts,
                polygons: poly_list,
                collision_tree: match shield_tree_chunk {
                    Some(chunk) => Some(*parse_shield_node(&chunk, self.version)?),
                    None => None,
                },
            }),
            (None, Some(_)) => unreachable!(),
            _ => None,
        };

        // now that all the subobjects shouldve have been slotted in, assert that they all exist
        let mut sub_objects = ObjVec(sub_objects.into_iter().map(|subobj_opt| subobj_opt.unwrap()).collect());

        for i in 0..sub_objects.len() {
            if let Some(parent) = sub_objects.0[i].parent {
                let id = sub_objects.0[i].obj_id;
                sub_objects[parent].children.push(id);
            }
        }

        for id in debris_objs {
            sub_objects[id].is_debris_model = true;
        }

        let mut textures = textures.unwrap_or_default();
        let untextured_idx = post_parse_fill_untextured_slot(&mut sub_objects, &mut textures);

        Ok(Model {
            version: self.version,
            header: header.expect("No header chunk found???"),
            sub_objects,
            textures,
            paths: paths.unwrap_or_default(),
            special_points: special_points.unwrap_or_default(),
            eye_points: eye_points.unwrap_or_default(),
            primary_weps: primary_weps.unwrap_or_default(),
            secondary_weps: secondary_weps.unwrap_or_default(),
            turrets,
            thruster_banks: thruster_banks.unwrap_or_default(),
            comments: comments.unwrap_or_default(),
            docking_bays: dock_points.unwrap_or_default(),
            insignias: insignias.unwrap_or_default(),
            glow_banks: glow_banks.unwrap_or_default(),
            visual_center: visual_center.unwrap_or_default(),
            shield_data,
            path_to_file: path.canonicalize().unwrap_or(path),
            untextured_idx,
        })
    }

    fn read_list<T>(&mut self, f: impl FnMut(&mut Self) -> io::Result<T>) -> io::Result<Vec<T>> {
        let n = self.read_u32()? as usize;
        self.read_list_n(n, f)
    }

    fn read_list_n<T>(&mut self, n: usize, mut f: impl FnMut(&mut Self) -> io::Result<T>) -> io::Result<Vec<T>> {
        (0..n).map(|_| f(self)).collect()
    }

    fn read_array<T, const N: usize>(&mut self, f: impl FnMut(&mut Self) -> io::Result<T>) -> io::Result<Box<[T; N]>> {
        Ok(self.read_list_n(N, f)?.into_boxed_slice().try_into().ok().unwrap())
    }

    fn read_string(&mut self) -> io::Result<String> {
        let buf = self.read_byte_buffer()?;
        let end = buf.iter().position(|&char| char == 0).unwrap_or(buf.len());
        Ok(String::from_utf8(buf[..end].into()).unwrap())
    }

    fn read_u32(&mut self) -> io::Result<u32> {
        Ok(u32::from_le_bytes(self.read_bytes()?))
    }

    fn read_i32(&mut self) -> io::Result<i32> {
        read_i32(&mut self.file)
    }

    fn read_byte_buffer(&mut self) -> io::Result<Box<[u8]>> {
        let mut buffer = vec![0; self.read_u32()? as usize];
        //println!("buffer size is {}", buffer.len());
        self.file.read_exact(&mut buffer)?;

        Ok(buffer.into())
    }

    fn read_bytes<const N: usize>(&mut self) -> io::Result<[u8; N]> {
        read_bytes(&mut self.file)
    }

    fn read_bbox(&mut self) -> io::Result<BoundingBox> {
        Ok(BoundingBox { min: self.read_vec3d()?, max: self.read_vec3d()? })
    }

    fn read_vec3d(&mut self) -> io::Result<Vec3d> {
        Ok(Vec3d {
            x: self.read_f32()?,
            y: self.read_f32()?,
            z: self.read_f32()?,
        })
    }

    fn read_f32(&mut self) -> io::Result<f32> {
        Ok(f32::from_le_bytes(self.read_bytes()?))
    }
}

fn read_i32(file: &mut impl Read) -> io::Result<i32> {
    Ok(i32::from_le_bytes(read_bytes(file)?))
}

fn read_bytes<const N: usize>(file: &mut impl Read) -> io::Result<[u8; N]> {
    let mut buffer = [0; N];
    file.read_exact(&mut buffer)?;

    Ok(buffer)
}

fn read_list_n<T>(n: usize, buf: &mut &[u8], mut f: impl FnMut(&mut &[u8]) -> io::Result<T>) -> io::Result<Vec<T>> {
    (0..n).map(|_| f(buf)).collect()
}

fn read_vec3d(buf: &mut &[u8]) -> io::Result<Vec3d> {
    Ok(Vec3d {
        x: buf.read_f32::<LE>()?,
        y: buf.read_f32::<LE>()?,
        z: buf.read_f32::<LE>()?,
    })
}

fn read_bbox(chunk: &mut &[u8]) -> io::Result<BoundingBox> {
    Ok(BoundingBox { min: read_vec3d(chunk)?, max: read_vec3d(chunk)? })
}

fn parse_chunk_header(buf: &[u8], chunk_type_is_u8: bool) -> io::Result<(u32, &[u8], &[u8])> {
    let mut pointer = buf;
    let chunk_type = if chunk_type_is_u8 {
        pointer.read_u8()?.into()
    } else {
        pointer.read_u32::<LE>()?
    };

    /*println!(
        "found a {}",
        match chunk_type {
            BspData::ENDOFBRANCH => "ENDOFBRANCH",
            BspData::DEFFPOINTS => "DEFFPOINTS",
            BspData::FLATPOLY => "FLATPOLY",
            BspData::TMAPPOLY => "TMAPPOLY",
            BspData::SORTNORM => "SORTNORM",
            BspData::SORTNORM2 => "SORTNORM2",
            BspData::TMAPPOLY2 => "TMAPPOLY2",
            _ => "no i dont",
        }
    );*/
    /*println!(
        "found a {}",
        match chunk_type {
            0 => "split",
            1 => "leaf",
            _ => "dunno lol",
        }
    );*/
    let chunk_size = pointer.read_u32::<LE>()? as usize;
    Ok((chunk_type, pointer, &buf[chunk_size..]))
}

fn parse_bsp_data(mut buf: &[u8], version: Version) -> io::Result<BspData> {
    fn parse_bsp_node(mut buf: &[u8], verts: &[Vec3d], version: Version) -> io::Result<Box<BspNode>> {
        // parse the first header
        let (chunk_type, mut chunk, next_chunk) = parse_chunk_header(buf, false)?;
        // the first chunk (after deffpoints) AND the chunks pointed to be SORTNORM's front and back branches should ALWAYS be either another
        // SORTNORM or a BOUNDBOX followed by some polygons
        // dbg!(chunk_type);
        Ok(Box::new(match chunk_type {
            BspData::SORTNORM | BspData::SORTNORM2 => BspNode::Split {
                front: {
                    if chunk_type == BspData::SORTNORM {
                        let _normal = read_vec3d(&mut chunk)?;
                        let _point = read_vec3d(&mut chunk)?;
                        let _reserved = chunk.read_u32::<LE>()?; // just to advance past it
                    }
                    let offset = chunk.read_u32::<LE>()?;
                    if offset == 0 {
                        Box::new(BspNode::Empty)
                    } else {
                        parse_bsp_node(&buf[offset as usize..], verts, version)?
                    }
                },
                back: {
                    let offset = chunk.read_u32::<LE>()?;
                    if offset == 0 {
                        Box::new(BspNode::Empty)
                    } else {
                        parse_bsp_node(&buf[offset as usize..], verts, version)?
                    }
                },
                bbox: {
                    if chunk_type == BspData::SORTNORM {
                        let _prelist = chunk.read_u32::<LE>()?; //
                        let _postlist = chunk.read_u32::<LE>()?; // All 3 completely unused, as far as I can tell
                        let _online = chunk.read_u32::<LE>()?; //
                    }
                    if version >= Version::V20_00 {
                        read_bbox(&mut chunk)?
                    } else {
                        BoundingBox::default()
                    }
                },
            },
            BspData::BOUNDBOX => {
                let bbox = read_bbox(&mut chunk)?;
                let mut poly_list = vec![];
                buf = next_chunk;
                loop {
                    let (chunk_type, mut chunk, next_chunk) = parse_chunk_header(buf, false)?;
                    // keeping looping and pushing new polygons
                    poly_list.push(match chunk_type {
                        BspData::TMAPPOLY => {
                            let normal = read_vec3d(&mut chunk)?;
                            let _center = read_vec3d(&mut chunk)?;
                            let _radius = chunk.read_f32::<LE>()?;
                            let num_verts = chunk.read_u32::<LE>()?;
                            let texture = TextureId(chunk.read_u32::<LE>()?);
                            let verts = read_list_n(num_verts as usize, &mut chunk, |chunk| {
                                Ok(PolyVertex {
                                    vertex_id: VertexId(chunk.read_u16::<LE>()?.into()),
                                    normal_id: NormalId(chunk.read_u16::<LE>()?.into()),
                                    uv: (chunk.read_f32::<LE>()?, chunk.read_f32::<LE>()?),
                                })
                            })?;

                            Polygon { normal, verts, texture }
                        }
                        BspData::FLATPOLY => {
                            let normal = read_vec3d(&mut chunk)?;
                            let _center = read_vec3d(&mut chunk)?;
                            let _radius = chunk.read_f32::<LE>()?;
                            let num_verts = chunk.read_u32::<LE>()?;
                            let texture = TextureId::UNTEXTURED;
                            let _color = chunk.read_u32::<LE>()?;
                            let verts = read_list_n(num_verts as usize, &mut chunk, |chunk| {
                                Ok(PolyVertex {
                                    vertex_id: VertexId(chunk.read_u16::<LE>()?.into()),
                                    normal_id: NormalId(chunk.read_u16::<LE>()?.into()),
                                    uv: Default::default(),
                                })
                            })?;

                            Polygon { normal, verts, texture }
                        }
                        BspData::ENDOFBRANCH => {
                            break;
                        }
                        _ => {
                            unreachable!("unknown chunk type! {}", chunk_type);
                        }
                    });

                    buf = next_chunk;
                }
                //println!("leaf length {}", poly_list.len());
                match poly_list.len() {
                    0 => BspNode::Empty,
                    1 => BspNode::Leaf { bbox, poly: poly_list.into_iter().next().unwrap() },
                    _ => BspData::recalculate(verts, poly_list.into_iter()),
                }
            }

            BspData::TMAPPOLY2 => {
                let bbox = read_bbox(&mut chunk)?;
                let poly = {
                    let normal = read_vec3d(&mut chunk)?;
                    let texture = TextureId(chunk.read_u32::<LE>()?);
                    let num_verts = chunk.read_u32::<LE>()?;
                    let verts = read_list_n(num_verts as usize, &mut chunk, |chunk| {
                        Ok(PolyVertex {
                            vertex_id: VertexId(chunk.read_u32::<LE>()?.into()),
                            normal_id: NormalId(chunk.read_u32::<LE>()?.into()),
                            uv: (chunk.read_f32::<LE>()?, chunk.read_f32::<LE>()?),
                        })
                    })?;

                    Polygon { normal, verts, texture }
                };
                BspNode::Leaf { bbox, poly }
            }
            BspData::ENDOFBRANCH => BspNode::Empty,
            _ => {
                unreachable!();
            }
        }))
    }

    //println!("started parsing a bsp tree");

    let (chunk_type, mut chunk, next_chunk) = parse_chunk_header(buf, false)?;
    assert!(chunk_type == BspData::DEFFPOINTS);

    let num_verts = chunk.read_u32::<LE>()?;
    let num_norms = chunk.read_u32::<LE>()?;
    let offset = chunk.read_u32::<LE>()?;
    let norm_counts = &chunk[0..num_verts as usize];

    buf = &buf[offset as usize..];

    let mut verts = vec![];
    let mut norms = vec![];
    for &count in norm_counts {
        verts.push(read_vec3d(&mut buf)?);
        for _ in 0..count {
            norms.push(read_vec3d(&mut buf)?);
        }
    }

    assert!(num_norms as usize == norms.len());

    let mut bsp_tree = *parse_bsp_node(next_chunk, &verts, version)?;

    if version < Version::V20_00 {
        bsp_tree.recalculate_bboxes(&verts);
    }

    Ok(BspData { collision_tree: bsp_tree, norms, verts })
}

fn parse_shield_node(buf: &[u8], version: Version) -> io::Result<Box<ShieldNode>> {
    let (chunk_type, mut chunk, _) = parse_chunk_header(buf, version < Version::V22_00)?;
    Ok(Box::new(match chunk_type {
        ShieldNode::SPLIT => ShieldNode::Split {
            bbox: read_bbox(&mut chunk)?,
            front: {
                let offset = chunk.read_u32::<LE>()?;
                assert!(offset != 0);
                parse_shield_node(&buf[offset as usize..], version)?
            },
            back: {
                let offset = chunk.read_u32::<LE>()?;
                assert!(offset != 0);
                parse_shield_node(&buf[offset as usize..], version)?
            },
        },
        ShieldNode::LEAF => ShieldNode::Leaf {
            bbox: read_bbox(&mut chunk)?,
            poly_list: read_list_n(chunk.read_u32::<LE>()? as usize, &mut chunk, |chunk| Ok(PolygonId(chunk.read_u32::<LE>()?)))?,
        },
        _ => unreachable!(),
    }))
}

// =================================================================
// DAE / glTF parsing
// =================================================================

trait IsNode<'a>: Clone {
    type Children: Iterator<Item = Self>;

    fn prepend_transforms(&self, mat: &mut Mat4x4);
    // fn on_children<'a>(&'a self, f: impl FnMut(&'a Self));
    fn name(&self) -> Option<&'a str>;
    fn children(&self) -> Self::Children;
    fn transform(&self) -> Mat4x4;

    // given a node, using its transforms return a position, normal and radius
    // things commonly needed by various pof points
    fn parse_point(&self, &(mut transform): &Mat4x4, up: UpAxis) -> (Vec3d, Vec3d, f32) {
        self.prepend_transforms(&mut transform);
        let zero = Vec3d::ZERO.into();
        let offset = transform.transform_point(&zero) - zero;
        let transform = transform.append_translation(&(-offset));
        let pos = Vec3d::from(offset).from_coord(up);
        let vector: Vec3d = transform.transform_point(&Vec3d::new(0., 0., 1.).to_coord(up).into()).into();
        let radius = vector.magnitude();
        let norm = vector.normalize().from_coord(up);
        (pos, norm, radius)
    }

    fn parse_properties(&self, properties: &mut String) {
        for node in self.children() {
            if let Some(name) = node.name() {
                if let Some(idx) = name.find(":") {
                    if properties.is_empty() {
                        *properties = format!("{}", &name[(idx + 1)..]);
                    } else {
                        *properties = format!("{}\n{}", properties, &name[(idx + 1)..]);
                    }
                }
            }
        }
    }
}

fn node_children_with_keyword<'a, N: IsNode<'a>>(node: N, keyword: &'a str) -> impl Iterator<Item = (N, &'a str)> {
    node.children().filter_map(move |node| {
        let name = node.name()?;
        if name.starts_with('#') && name.contains(keyword) {
            Some((node, name))
        } else {
            None
        }
    })
}

fn mk_insignia(detail_level: Option<u32>, offset: Vec3d, vertices: Vec<Vec3d>, polygons_out: Vec<(TextureId, Vec<PolyVertex>)>) -> Insignia {
    let mut faces = vec![];
    for (_, verts) in polygons_out {
        if let [vert1, ref rest @ ..] = &*verts {
            for slice in rest.windows(2) {
                if let [vert2, vert3] = slice {
                    faces.push((
                        PolyVertex { vertex_id: vert1.vertex_id, normal_id: (), uv: vert1.uv },
                        PolyVertex { vertex_id: vert2.vertex_id, normal_id: (), uv: vert2.uv },
                        PolyVertex { vertex_id: vert3.vertex_id, normal_id: (), uv: vert3.uv },
                    ))
                }
            }
        }
    }
    Insignia {
        detail_level: detail_level.unwrap_or(0),
        vertices,
        offset,
        faces,
    }
}

fn push_subobj(
    sub_objects: &mut Vec<SubObject>, offset: Vec3d, parent: Option<ObjectId>, name: &str, is_debris_model: bool, verts: Vec<Vec3d>,
    norms: Vec<Vec3d>, polygons: Vec<(TextureId, Vec<PolyVertex>)>,
) -> ObjectId {
    let obj_id = ObjectId(sub_objects.len() as _);

    let mut new_subobj = SubObject {
        obj_id,
        radius: Default::default(),
        parent,
        offset,
        geo_center: offset,
        bbox: Default::default(),
        name: name.to_string(),
        properties: Default::default(),
        movement_type: Default::default(),
        movement_axis: Default::default(),
        bsp_data: BspData {
            norms,
            collision_tree: BspData::recalculate(
                &verts,
                polygons
                    .into_iter()
                    .map(|(texture, verts)| Polygon { normal: Default::default(), texture, verts }),
            ),
            verts,
        },
        children: Default::default(),
        is_debris_model,
    };

    new_subobj.recalc_bbox();
    new_subobj.recalc_radius();

    sub_objects.push(new_subobj);
    obj_id
}

trait ParseCtx<'a> {
    type Node: IsNode<'a>;
    fn up(&self) -> UpAxis {
        UpAxis::YUp
    }

    fn parse_geometry(&self, node: &Self::Node, transform: &Mat4x4) -> (Vec<Vec3d>, Vec<Vec3d>, Vec<(TextureId, Vec<PolyVertex>)>);

    fn parse_subobject_recursive(&self, model: &mut Model, node: Self::Node, parent: ObjectId, detail_level: Option<u32>, parent_transform: &Mat4x4) {
        let name = match node.name() {
            None => return, // subobjects must have names!
            Some(name) => name,
        };
        let mut transform = parent_transform * node.transform();
        let zero = Vec3d::ZERO.into();
        let center = transform.transform_point(&zero) - zero;
        transform.append_translation_mut(&(-center));
        let up = self.up();
        let offset = Vec3d::from(center).from_coord(up);

        let (vertices_out, normals_out, polygons_out) = self.parse_geometry(&node, &transform);

        // ignore subobjects with no geo
        // metadata (empties with names like #properties) are handled below directly
        // this function must *start* with a proper subobject
        if polygons_out.is_empty() {
            return;
        }

        if name.to_lowercase().contains("insig") {
            model.insignias.push(mk_insignia(detail_level, offset, vertices_out, polygons_out));
        } else {
            // this should probably be warned about...
            if vertices_out.is_empty() || normals_out.is_empty() {
                return;
            }

            let obj_id = push_subobj(&mut model.sub_objects, offset, Some(parent), name, false, vertices_out, normals_out, polygons_out);

            for node in node.children() {
                // make a pointer to the subobj we just pushed
                // annoying, but the node children could be properties that modify it, or proper subobject children
                // which will require that this subobject be already pushed into the list
                let len = model.sub_objects.len() - 1;
                let subobj = &mut model.sub_objects.0[len];

                if let Some(name) = node.name() {
                    if name.starts_with('#') && name.contains("point") {
                        let turret = {
                            match model.turrets.iter().position(|turret| turret.gun_obj == obj_id) {
                                Some(idx) => &mut model.turrets[idx],
                                None => {
                                    model.turrets.push(Turret::default());
                                    let idx = model.turrets.len() - 1;
                                    &mut model.turrets[idx]
                                }
                            }
                        };

                        turret.gun_obj = obj_id;
                        turret.base_obj = if name.contains("gun") { parent } else { obj_id };

                        let (pos, norm, _) = node.parse_point(&transform, up);
                        turret.fire_points.push(pos);
                        turret.normal = norm.try_into().unwrap_or_default();
                        continue;
                    } else if name.starts_with('#') && name.contains("properties") {
                        node.parse_properties(&mut subobj.properties);
                        continue;
                    } else if name.starts_with('#') && name.contains("mov-type") {
                        if let Some(idx) = name.find(':') {
                            if let Ok(val) = name[(idx + 1)..].parse::<i32>() {
                                subobj.movement_type = val.try_into().unwrap_or_default();
                            }
                        }
                        continue;
                    } else if name.starts_with('#') && name.contains("mov-axis") {
                        if let Some(idx) = name.find(':') {
                            if let Ok(val) = name[(idx + 1)..].parse::<i32>() {
                                subobj.movement_axis = val.try_into().unwrap_or_default();
                            }
                        }
                        continue;
                    }
                }

                self.parse_subobject_recursive(model, node, obj_id, detail_level, &transform);
            }
        }
    }

    fn parse_top_level_nodes(&self, model: &mut Model, nodes: impl IntoIterator<Item = Self::Node>) {
        for node in nodes {
            let mut transform = node.transform();
            let zero = Vec3d::ZERO.into();
            let center = transform.transform_point(&zero) - zero;
            transform.append_translation_mut(&(-center));
            let up = self.up();
            let offset = Vec3d::from(center).from_coord(up);

            let name = match node.name() {
                Some(name) => name,
                None => continue,
            };

            let (vertices_out, normals_out, polygons_out) = self.parse_geometry(&node, &transform);
            if !polygons_out.is_empty() {
                if name.to_lowercase() == "shield" {
                    let mut polygons = vec![];
                    for (_, verts) in polygons_out {
                        let verts = verts.into_iter().map(|poly| poly.vertex_id).collect::<Vec<_>>();
                        // triangulate, just to be sure
                        if let [vert1, ref rest @ ..] = *verts {
                            for slice in rest.windows(2) {
                                if let [vert2, vert3] = *slice {
                                    let [v1, v2, v3] = [vert1, vert2, vert3].map(|i| nalgebra_glm::Vec3::from(vertices_out[i.0 as usize]));
                                    polygons.push(ShieldPolygon {
                                        normal: (v2 - v1).cross(&(v3 - v1)).normalize().into(),
                                        verts: (vert1, vert2, vert3),
                                        neighbors: Default::default(),
                                    })
                                }
                            }
                        }
                    }

                    // assign shield neighbors
                    // create a map keyed on each vertex pair, based on winding order, where the value is the polygon id
                    let mut map: HashMap<(VertexId, VertexId), PolygonId> = HashMap::new();
                    for (i, poly) in polygons.iter().enumerate() {
                        map.insert((poly.verts.0, poly.verts.1), PolygonId(i as u32));
                        map.insert((poly.verts.1, poly.verts.2), PolygonId(i as u32));
                        map.insert((poly.verts.2, poly.verts.0), PolygonId(i as u32));
                    }

                    // for each polygon then, by swapping its vertex pairs, you can grab each adjacent polygon
                    for poly in &mut polygons {
                        let neighbor1 = map.get(&(poly.verts.1, poly.verts.0)).unwrap_or(&PolygonId(0));
                        let neighbor2 = map.get(&(poly.verts.2, poly.verts.1)).unwrap_or(&PolygonId(0));
                        let neighbor3 = map.get(&(poly.verts.0, poly.verts.2)).unwrap_or(&PolygonId(0));
                        poly.neighbors = (*neighbor1, *neighbor2, *neighbor3);
                    }
                    // a map insertion where an entry already exists or a failure to get from the map indicate non-manifoldness, TODO maybe indicate that

                    model.shield_data = Some(ShieldData {
                        collision_tree: Some(ShieldData::recalculate_tree(&vertices_out, &polygons)),
                        verts: vertices_out,
                        polygons,
                    });
                } else if name.to_lowercase().contains("insig") {
                    model.insignias.push(mk_insignia(None, offset, vertices_out, polygons_out));
                } else {
                    // must be a subobject

                    // this should probably be warned about...
                    if vertices_out.is_empty() || normals_out.is_empty() {
                        continue;
                    }

                    let obj_id =
                        push_subobj(&mut model.sub_objects, offset, None, name, name.starts_with("debris"), vertices_out, normals_out, polygons_out);

                    let mut detail_level: Option<u32> = None;
                    if let Some(idx) = name.to_lowercase().find("detail") {
                        if let Ok(level) = name[(idx + 6)..].parse::<usize>() {
                            if level >= model.header.detail_levels.len() {
                                model.header.detail_levels.resize(level + 1, obj_id);
                            } else {
                                model.header.detail_levels[level] = obj_id;
                            }
                            detail_level = Some(level as u32);
                        }
                    }

                    for node in node.children() {
                        self.parse_subobject_recursive(model, node, obj_id, detail_level, &transform);
                    }
                }
            } else if name == "#thrusters" {
                for (node, _) in node_children_with_keyword(node, "bank") {
                    let mut new_bank = ThrusterBank::default();

                    for (node, name) in node_children_with_keyword(node, "") {
                        if name.contains("properties") {
                            node.parse_properties(&mut new_bank.properties);
                        } else if name.contains("point") {
                            let mut new_point = ThrusterGlow::default();

                            let (pos, norm, rad) = node.parse_point(&transform, up);
                            new_point.position = pos;
                            new_point.normal = norm;
                            new_point.radius = rad;

                            new_bank.glows.push(new_point);
                        }
                    }

                    model.thruster_banks.push(new_bank);
                }
            } else if name == "#paths" {
                for (node, _) in node_children_with_keyword(node, "path") {
                    let mut new_path = Path::default();

                    for (node, name) in node_children_with_keyword(node, "") {
                        if name.contains("parent") {
                            if let Some(idx) = name.find(':') {
                                new_path.parent = format!("{}", &name[(idx + 1)..]);
                            }
                        } else if name.contains("name") {
                            if let Some(idx) = name.find(':') {
                                new_path.name = format!("{}", &name[(idx + 1)..]);
                            }
                        } else if name.contains("point") {
                            let mut new_point = PathPoint::default();

                            let (pos, _, rad) = node.parse_point(&transform, up);
                            new_point.position = pos;
                            new_point.radius = rad;

                            new_path.points.push(new_point);
                        }
                    }

                    model.paths.push(new_path);
                }
            } else if name.starts_with("#") && name.contains("weapons") {
                for (node, _) in node_children_with_keyword(node, "bank") {
                    let mut new_bank = vec![];

                    for (node, _) in node_children_with_keyword(node, "point") {
                        let mut new_point = WeaponHardpoint::default();

                        let (pos, norm, _) = node.parse_point(&transform, up);
                        new_point.position = pos;
                        new_point.normal = norm.try_into().unwrap_or_default();

                        for (_, name) in node_children_with_keyword(node, "offset") {
                            if let Some(idx) = name.find(":") {
                                if let Ok(val) = &name[(idx + 1)..].parse() {
                                    new_point.offset = *val;
                                    break;
                                }
                            }
                        }

                        new_bank.push(new_point);
                    }

                    if name.contains("secondary") {
                        model.secondary_weps.push(new_bank);
                    } else {
                        model.primary_weps.push(new_bank);
                    }
                }
            } else if name == "#docking bays" {
                for (node, _) in node_children_with_keyword(node, "bay") {
                    let mut new_bay = Dock::default();

                    let transform = node.transform();
                    let zero = Vec3d::ZERO.into();
                    new_bay.position = Vec3d::from(transform.transform_point(&zero) - zero).from_coord(up);
                    new_bay.fvec = transform.transform_vector(&glm::vec3(0., 1., 0.)).try_into().unwrap_or_default();
                    new_bay.fvec.0 = new_bay.fvec.0.from_coord(up);

                    new_bay.uvec = transform.transform_vector(&glm::vec3(0., 0., 1.)).try_into().unwrap_or_default();
                    new_bay.uvec.0 = new_bay.uvec.0.from_coord(up);

                    for (node, name) in node_children_with_keyword(node, "") {
                        if name.contains("properties") {
                            node.parse_properties(&mut new_bay.properties);
                        } else if name.contains("path") {
                            if let Some(idx) = name.find(":") {
                                if let Ok(val) = &name[(idx + 1)..].parse() {
                                    new_bay.path = Some(PathId(*val));
                                }
                            }
                        }
                    }

                    model.docking_bays.push(new_bay);
                }
            } else if name == "#glows" {
                for (node, _) in node_children_with_keyword(node, "glowbank") {
                    let mut new_bank = GlowPointBank::default();

                    for (node, name) in node_children_with_keyword(node, "") {
                        if name.contains("type") {
                            if let Some(idx) = name.find(":") {
                                if let Ok(val) = &name[(idx + 1)..].parse() {
                                    new_bank.glow_type = *val;
                                }
                            }
                        } else if name.contains("lod") {
                            if let Some(idx) = name.find(":") {
                                if let Ok(val) = &name[(idx + 1)..].parse() {
                                    new_bank.lod = *val;
                                }
                            }
                        } else if name.contains("parent") {
                            if let Some(idx) = name.find(":") {
                                if let Ok(val) = &name[(idx + 1)..].parse() {
                                    new_bank.obj_parent = ObjectId(*val);
                                }
                            }
                        } else if name.contains("ontime") {
                            if let Some(idx) = name.find(":") {
                                if let Ok(val) = &name[(idx + 1)..].parse() {
                                    new_bank.on_time = *val;
                                }
                            }
                        } else if name.contains("offtime") {
                            if let Some(idx) = name.find(":") {
                                if let Ok(val) = &name[(idx + 1)..].parse() {
                                    new_bank.off_time = *val;
                                }
                            }
                        } else if name.contains("disptime") {
                            if let Some(idx) = name.find(":") {
                                if let Ok(val) = &name[(idx + 1)..].parse() {
                                    new_bank.disp_time = *val;
                                }
                            }
                        } else if name.contains("properties") {
                            node.parse_properties(&mut new_bank.properties);
                        } else if name.contains("point") {
                            let mut new_point = GlowPoint::default();

                            let (pos, norm, rad) = node.parse_point(&transform, up);
                            new_point.position = pos;
                            new_point.normal = if name.contains("omni") { Vec3d::ZERO } else { norm };
                            new_point.radius = rad;

                            new_bank.glow_points.push(new_point);
                        }
                    }

                    model.glow_banks.push(new_bank);
                }
            } else if name == "#special points" {
                for (node, name) in node_children_with_keyword(node, "") {
                    let mut new_point = SpecialPoint::default();

                    if let Some(idx) = name.find(":") {
                        new_point.name = format!("{}", &name[(idx + 1)..]);
                    }

                    let (pos, _, rad) = node.parse_point(&transform, up);
                    new_point.position = pos;
                    new_point.radius = rad;

                    for (node, _) in node_children_with_keyword(node, "properties") {
                        node.parse_properties(&mut new_point.properties);
                    }

                    model.special_points.push(new_point);
                }
            } else if name == "#eye points" {
                for (node, _) in node_children_with_keyword(node, "point") {
                    let mut new_point = EyePoint::default();

                    let (pos, norm, _) = node.parse_point(&transform, up);
                    new_point.offset = pos;
                    new_point.normal = norm.try_into().unwrap_or_default();

                    for (_, name) in node_children_with_keyword(node, "parent") {
                        if let Some(idx) = name.find(":") {
                            if let Ok(val) = &name[(idx + 1)..].parse() {
                                new_point.attached_subobj = ObjectId(*val);
                                break;
                            }
                        }
                    }

                    model.eye_points.push(new_point);
                }
            } else if name == "#visual-center" {
                let (pos, _, _) = node.parse_point(&transform, up);
                model.visual_center = pos;
            }
        }

        for i in 0..model.sub_objects.len() {
            if let Some(parent) = model.sub_objects[ObjectId(i as u32)].parent {
                let id = model.sub_objects[ObjectId(i as u32)].obj_id;
                model.sub_objects[parent].children.push(id);
            }
        }

        if model.header.detail_levels.is_empty() && !model.sub_objects.is_empty() {
            model.header.detail_levels.push(ObjectId(0));
            // this is pretty bad, but not having any detail levels is worse
        }

        model.header.num_subobjects = model.sub_objects.len() as _;

        model.untextured_idx = post_parse_fill_untextured_slot(&mut model.sub_objects, &mut model.textures);

        model.recalc_radius();
        model.recalc_bbox();
        model.recalc_mass();
        model.recalc_moi();
    }
}

// =================================================================
// DAE parsing
// =================================================================

impl<'a> IsNode<'a> for &'a dae::Node {
    type Children = std::slice::Iter<'a, dae::Node>;
    fn prepend_transforms(&self, mat: &mut Mat4x4) {
        (*self).prepend_transforms(mat)
    }
    fn name(&self) -> Option<&'a str> {
        self.name.as_deref()
    }
    fn children(&self) -> Self::Children {
        self.children.iter()
    }
    fn transform(&self) -> Mat4x4 {
        self.transform_as_matrix()
    }
}

struct DaeContext<'a> {
    local_maps: dae::LocalMaps<'a>,
    material_map: HashMap<String, TextureId>,
    up: UpAxis,
}

impl<'a> ParseCtx<'a> for DaeContext<'a> {
    type Node = &'a dae::Node;
    fn up(&self) -> UpAxis {
        self.up
    }
    fn parse_geometry(&self, node: &&'a dae::Node, transform: &Mat4x4) -> (Vec<Vec3d>, Vec<Vec3d>, Vec<(TextureId, Vec<PolyVertex>)>) {
        let mut vertices_out: Vec<Vec3d> = vec![];
        let mut normals_out: Vec<Vec3d> = vec![];
        let mut normals_map: HashMap<Vec3d, NormalId> = HashMap::new();
        let mut polygons_out = vec![];

        for geo in &node.instance_geometry {
            use dae::source::{SourceReader, ST, XYZ};

            struct VertexContext {
                vertex_offset: u32,
                normal_ids: Vec<NormalId>,
            }

            impl<'a> dae::geom::VertexLoad<'a, VertexContext> for PolyVertex {
                fn position(ctx: &VertexContext, _: &SourceReader<'a, XYZ>, index: u32) -> Self {
                    PolyVertex {
                        vertex_id: VertexId(index + ctx.vertex_offset),
                        normal_id: NormalId(0),
                        uv: (0.0, 0.0),
                    }
                }
                fn add_normal(&mut self, ctx: &VertexContext, _: &SourceReader<'a, XYZ>, index: u32) {
                    self.normal_id = ctx.normal_ids[index as usize];
                }
                fn add_texcoord(&mut self, _: &VertexContext, reader: &SourceReader<'a, ST>, index: u32, set: Option<u32>) {
                    // maybe raise an import warning over this?
                    if set == Some(0) {
                        let [u, v] = reader.get(index as usize);
                        self.uv = (u, 1. - v);
                    }
                }
            }
            let instance_mat_map: HashMap<_, _> = geo
                .instance_materials()
                .iter()
                .map(|x| {
                    (
                        x.symbol.clone(),
                        match &x.target.val {
                            dae::Url::Fragment(r) => r.clone(),
                            dae::Url::Other(_) => panic!(),
                        },
                    )
                })
                .collect();

            let geo = self.local_maps[&geo.url].element.as_mesh().unwrap();
            let verts = geo.vertices.as_ref().unwrap().importer(&self.local_maps).unwrap();
            let mut vert_ctx = VertexContext { vertex_offset: vertices_out.len() as u32, normal_ids: vec![] };

            for position in Clone::clone(verts.position_importer().unwrap()).map(Vec3d::from) {
                vertices_out.push((transform * position).from_coord(self.up));
            }

            for prim_elem in &geo.elements {
                match prim_elem {
                    dae::Primitive::PolyList(polies) => {
                        //println!("{:#?}, {:#?}, {:#?}", polies.material, instance_mat_map, self.material_map);
                        let texture = match &polies.material {
                            Some(mat) => self.material_map[&instance_mat_map[mat]],
                            None => TextureId::UNTEXTURED,
                        };

                        let importer = polies.importer(&self.local_maps, verts.clone()).unwrap();

                        vert_ctx.normal_ids = vec![];
                        if let Some(normal_importer) = importer.normal_importer() {
                            for normal in Clone::clone(normal_importer).map(Vec3d::from) {
                                vert_ctx.normal_ids.push(*normals_map.entry(normal).or_insert_with(|| {
                                    let id = NormalId(normals_out.len().try_into().unwrap());
                                    normals_out.push((transform * normal).from_coord(self.up));
                                    id
                                }));
                            }
                        }

                        let mut iter = importer.read::<_, PolyVertex>(&vert_ctx, &polies.data.prim);

                        for &n in &*polies.data.vcount {
                            let verts = (0..n).map(|_| iter.next().unwrap()).collect();
                            polygons_out.push((texture, verts));
                        }
                    }
                    dae::Primitive::Triangles(tris) => {
                        //println!("{:#?}, {:#?}, {:#?}", tris.material, instance_mat_map, self.material_map);
                        let texture = match &tris.material {
                            Some(mat) => self.material_map[&instance_mat_map[mat]],
                            None => TextureId::UNTEXTURED,
                        };
                        let importer = tris.importer(&self.local_maps, verts.clone()).unwrap();

                        vert_ctx.normal_ids = vec![];
                        if let Some(normal_importer) = importer.normal_importer() {
                            for normal in Clone::clone(normal_importer) {
                                vert_ctx.normal_ids.push(*normals_map.entry(normal.into()).or_insert_with(|| {
                                    let id = NormalId(normals_out.len().try_into().unwrap());
                                    normals_out.push((transform * Vec3d::from(normal)).from_coord(self.up));
                                    id
                                }));
                            }
                        }

                        let mut iter = importer.read::<_, PolyVertex>(&vert_ctx, tris.data.prim.as_ref().unwrap());
                        while let Some(vert1) = iter.next() {
                            polygons_out.push((texture, vec![vert1, iter.next().unwrap(), iter.next().unwrap()]));
                        }
                    }
                    _ => {}
                }
            }
        }

        for poly in &mut polygons_out {
            poly.1.reverse(); // normal facing (which is determined by winding order) is inverted for FSO
        }

        (vertices_out, normals_out, polygons_out)
    }
}

pub fn parse_dae(path: std::path::PathBuf) -> Box<Model> {
    let document = dae::Document::from_file(&path).unwrap();
    // use std::io::Write;
    // write!(std::fs::File::create("output.log").unwrap(), "{:#?}", document).unwrap();
    let mut ctx = DaeContext {
        local_maps: document.local_maps(),
        material_map: HashMap::new(),
        up: document.asset.up_axis,
    };

    document.for_each(|material: &dae::Material| {
        ctx.material_map
            .insert(material.id.as_ref().unwrap().clone(), TextureId(ctx.material_map.len() as u32));
    });

    let mut model = Box::new(Model::default());
    model.path_to_file = path.canonicalize().unwrap_or(path);
    model.textures = vec![String::new(); ctx.material_map.len()];
    for (tex, id) in &ctx.material_map {
        model.textures[id.0 as usize] = tex.strip_suffix("-material").unwrap_or(tex).to_string();
    }

    let scene = &document.scene.as_ref().unwrap().instance_visual_scene.as_ref().unwrap().url;
    ctx.parse_top_level_nodes(&mut model, &ctx.local_maps.get(scene).unwrap().nodes);
    model
}

// =================================================================
// glTF parsing
// =================================================================

impl<'a> IsNode<'a> for gltf::Node<'a> {
    type Children = gltf::scene::iter::Children<'a>;
    fn prepend_transforms(&self, mat: &mut Mat4x4) {
        *mat = *mat * Mat4x4::from(self.transform().matrix());
    }
    fn name(&self) -> Option<&'a str> {
        self.name()
    }
    fn children(&self) -> Self::Children {
        self.children()
    }
    fn transform(&self) -> Mat4x4 {
        self.transform().matrix().into()
    }
}

struct GltfContext {
    buffers: Vec<gltf::buffer::Data>,
}

impl<'a> ParseCtx<'a> for GltfContext {
    type Node = gltf::Node<'a>;
    fn parse_geometry(&self, node: &Self::Node, transform: &Mat4x4) -> (Vec<Vec3d>, Vec<Vec3d>, Vec<(TextureId, Vec<PolyVertex>)>) {
        let mut vertices_out: Vec<Vec3d> = vec![];
        let mut vertices_map: HashMap<Vec3d, VertexId> = HashMap::new();
        let mut normals_out: Vec<Vec3d> = vec![];
        let mut normals_map: HashMap<Vec3d, NormalId> = HashMap::new();
        let mut polygons_out = vec![];

        if let Some(mesh) = node.mesh() {
            for primitive in mesh.primitives() {
                let reader = primitive.reader(|b| Some(&self.buffers[b.index()]));
                let mut vertex_ids = vec![];
                for position in reader.read_positions().unwrap().map(Vec3d::from) {
                    vertex_ids.push(*vertices_map.entry(position).or_insert_with(|| {
                        let id = VertexId(vertices_out.len().try_into().unwrap());
                        vertices_out.push((transform * position).from_coord(UpAxis::YUp));
                        id
                    }));
                }
                let texture = match primitive.material().index() {
                    Some(idx) => TextureId(idx as u32),
                    None => TextureId::UNTEXTURED,
                };
                let uvs = reader
                    .read_tex_coords(0)
                    .map(|iter| iter.into_f32().map(|[u, v]| (u, v)).collect::<Vec<_>>());

                match primitive.mode() {
                    gltf::mesh::Mode::Triangles => {
                        let mut normal_ids = vec![];
                        if let Some(normal_iter) = reader.read_normals() {
                            for normal in normal_iter.map(Vec3d::from) {
                                normal_ids.push(*normals_map.entry(normal).or_insert_with(|| {
                                    let id = NormalId(normals_out.len().try_into().unwrap());
                                    normals_out.push((transform * normal).from_coord(UpAxis::YUp));
                                    id
                                }));
                            }
                        }

                        macro_rules! on_indices {
                            ($indices:expr) => {{
                                let mut iter = $indices.map(|i| PolyVertex {
                                    vertex_id: vertex_ids[i as usize],
                                    normal_id: normal_ids.get(i as usize).copied().unwrap_or_default(),
                                    uv: uvs.as_ref().map_or((0., 0.), |vec| vec[i as usize]),
                                });
                                while let Some(vert1) = iter.next() {
                                    polygons_out.push((texture, vec![vert1, iter.next().unwrap(), iter.next().unwrap()]));
                                }
                            }};
                        }
                        match reader.read_indices() {
                            Some(indices) => on_indices!(indices.into_u32()),
                            None => on_indices!(0..vertex_ids.len() as u32),
                        }
                    }
                    _ => unreachable!(),
                }
            }
        }

        for poly in &mut polygons_out {
            poly.1.reverse(); // normal facing (which is determined by winding order) is inverted for FSO
        }

        (vertices_out, normals_out, polygons_out)
    }
}

pub fn parse_gltf(path: std::path::PathBuf) -> Box<Model> {
    let base = path.parent().unwrap_or_else(|| std::path::Path::new("./"));
    let reader = BufReader::new(File::open(&path).unwrap());
    let gltf::Gltf { document: gltf, blob } = gltf::Gltf::from_reader(reader).unwrap();
    // let buffers = gltf::import::import_buffer_data(&gltf, Some(base), blob).unwrap();
    let buffers = import_buffer_data(&gltf, &base, blob);
    let mut model = Box::new(Model::default());
    model.path_to_file = path.canonicalize().unwrap_or(path);
    model.textures = gltf
        .materials()
        .map(|mat| mat.name().unwrap().strip_suffix("-material").unwrap_or(mat.name().unwrap()).to_string())
        .collect();

    GltfContext { buffers }.parse_top_level_nodes(&mut model, gltf.default_scene().unwrap().nodes());
    model
}

// Polyfill for https://github.com/gltf-rs/gltf/pull/341 - delete me
fn import_buffer_data(document: &gltf::Document, base: &std::path::Path, mut blob: Option<Vec<u8>>) -> Vec<gltf::buffer::Data> {
    fn read_to_end(path: impl AsRef<std::path::Path>) -> Vec<u8> {
        let file = File::open(path.as_ref()).unwrap();
        let length = file.metadata().map(|x| x.len() + 1).unwrap_or(0);
        let mut reader = io::BufReader::new(file);
        let mut data = Vec::with_capacity(length as usize);
        reader.read_to_end(&mut data).unwrap();
        data
    }

    let mut buffers = Vec::new();
    for buffer in document.buffers() {
        let mut data = match buffer.source() {
            gltf::buffer::Source::Uri(uri) => {
                if uri.contains(':') {
                    if let Some(rest) = uri.strip_prefix("data:") {
                        let mut it = rest.split(";base64,");
                        match (it.next(), it.next()) {
                            (_, Some(base64)) | (Some(base64), _) => base64::decode(&base64).unwrap(),
                            _ => panic!(),
                        }
                    } else if let Some(rest) = uri.strip_prefix("file://") {
                        read_to_end(rest)
                    } else if let Some(rest) = uri.strip_prefix("file:") {
                        read_to_end(rest)
                    } else {
                        panic!()
                    }
                } else {
                    read_to_end(base.join(&*urlencoding::decode(uri).unwrap()))
                }
            }
            gltf::buffer::Source::Bin => blob.take().unwrap(),
        };
        while data.len() % 4 != 0 {
            data.push(0);
        }
        assert!(data.len() >= buffer.length());
        buffers.push(gltf::buffer::Data(data));
    }
    buffers
}
