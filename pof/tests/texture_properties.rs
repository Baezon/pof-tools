//! Tests for the per-texture properties (TXPR) chunk surviving a write and a parse.

use pof::*;
use std::io::Cursor;
use std::path::PathBuf;

fn model_with_textures(textures: &[&str], properties: &[&str]) -> Model {
    let mut model = Model::default();
    model.version = Version::V23_01;
    model.textures = textures.iter().map(|tex| tex.to_string()).collect();
    model.texture_properties = properties.iter().map(|props| props.to_string()).collect();
    model
}

fn write_bytes(model: &Model) -> Vec<u8> {
    let mut bytes = vec![];
    model.write(&mut bytes).unwrap();
    bytes
}

fn round_trip(model: &Model) -> Model {
    Parser::new(Cursor::new(write_bytes(model))).unwrap().parse(PathBuf::from("test.pof")).unwrap()
}

fn chunk_ids(bytes: &[u8]) -> Vec<[u8; 4]> {
    // walks the chunk headers after the 8 byte file header, so a chunk id inside a string can't count
    let mut ids = vec![];
    let mut pos = 8;
    while pos + 8 <= bytes.len() {
        ids.push(bytes[pos..pos + 4].try_into().unwrap());
        pos += 8 + u32::from_le_bytes(bytes[pos + 4..pos + 8].try_into().unwrap()) as usize;
    }
    ids
}

fn has_chunk(bytes: &[u8], id: &[u8; 4]) -> bool {
    chunk_ids(bytes).contains(id)
}

/// appends a TXPR chunk by hand, since the writer never produces one that disagrees with TXTR
fn append_txpr(bytes: &mut Vec<u8>, properties: &[&str]) {
    let mut body = (properties.len() as u32).to_le_bytes().to_vec();
    for props in properties {
        let padded_len = (props.len() / 4 + 1) * 4;
        body.extend((padded_len as u32).to_le_bytes());
        body.extend(props.as_bytes());
        body.resize(body.len() + padded_len - props.len(), 0);
    }
    bytes.extend(b"TXPR");
    bytes.extend((body.len() as u32).to_le_bytes());
    bytes.extend(body);
}

fn parse_bytes(bytes: Vec<u8>) -> Model {
    Parser::new(Cursor::new(bytes)).unwrap().parse(PathBuf::from("test.pof")).unwrap()
}

#[test]
fn properties_survive_a_round_trip() {
    let model = model_with_textures(&["hull", "glow", "decal"], &["$glow_intensity=2.0", "", "$a=1\n$b=2"]);
    let parsed = round_trip(&model);
    assert_eq!(parsed.textures, model.textures);
    assert_eq!(parsed.texture_properties, model.texture_properties);
}

#[test]
fn a_short_list_is_padded_to_one_entry_per_texture() {
    // a missing entry means "no properties", so the file always holds one string per texture
    let parsed = round_trip(&model_with_textures(&["hull", "glow", "decal"], &["$glow_intensity=2.0"]));
    assert_eq!(parsed.texture_properties, vec!["$glow_intensity=2.0", "", ""]);
}

#[test]
fn no_chunk_is_written_without_properties() {
    // models nobody gave properties to stay byte for byte what older versions wrote
    let without = model_with_textures(&["hull", "glow"], &[]);
    let all_empty = model_with_textures(&["hull", "glow"], &["", ""]);
    assert!(!has_chunk(&write_bytes(&without), b"TXPR"));
    assert_eq!(write_bytes(&all_empty), write_bytes(&without));
    assert!(round_trip(&all_empty).texture_properties.is_empty());
}

#[test]
fn the_chunk_is_written_when_any_texture_has_properties() {
    let model = model_with_textures(&["hull", "glow"], &["", "$x=1"]);
    assert!(has_chunk(&write_bytes(&model), b"TXPR"));
    assert_eq!(round_trip(&model).texture_properties, vec!["", "$x=1"]);
}

#[test]
fn entries_past_the_last_texture_are_not_written() {
    let parsed = round_trip(&model_with_textures(&["hull"], &["$a=1", "$b=2"]));
    assert_eq!(parsed.texture_properties, vec!["$a=1"]);

    // only the extra entry has properties, so there is nothing to write
    let orphan_only = model_with_textures(&["hull"], &["", "$orphan=1"]);
    assert_eq!(write_bytes(&orphan_only), write_bytes(&model_with_textures(&["hull"], &[])));
}

#[test]
fn a_longer_txpr_is_cut_to_the_texture_count_on_parse() {
    let mut bytes = write_bytes(&model_with_textures(&["hull"], &[]));
    append_txpr(&mut bytes, &["$a=1", "$stray=2"]);
    assert_eq!(parse_bytes(bytes).texture_properties, vec!["$a=1"]);
}

#[test]
fn long_properties_are_not_truncated() {
    // curves are expected to need several thousand characters, far past the 256 of submodel properties
    let long = "$curve=".to_string() + &"0.5, ".repeat(1200);
    assert!(long.len() > 5000);
    let parsed = round_trip(&model_with_textures(&["hull"], &[&long]));
    assert_eq!(parsed.texture_properties, vec![long]);
}
