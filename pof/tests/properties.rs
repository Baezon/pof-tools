//! Tests for reading, updating and deleting `$field=value` entries in a properties string.

use pof::*;

// ---------------------------------------------------------------- reading

#[test]
fn a_value_runs_to_the_end_of_its_line() {
    let props = "$name=Fighterbay\n$parent_submodel=hull\n$special=subsystem";
    assert_eq!(properties_get_field(props, "$name"), Some("Fighterbay"));
    assert_eq!(properties_get_field(props, "$parent_submodel"), Some("hull"));
    assert_eq!(properties_get_field(props, "$special"), Some("subsystem"));
    assert_eq!(properties_get_field(props, "$nothing"), None);
}

#[test]
fn a_value_may_be_separated_by_equals_colon_or_whitespace() {
    // FSO's get_user_prop_value skips '=', ':' and whitespace before reading the value
    for props in ["$name=bay", "$name:bay", "$name bay", "$name = bay", "$name:  bay", "$name   =   bay"] {
        assert_eq!(properties_get_field(props, "$name"), Some("bay"), "for {:?}", props);
    }
}

#[test]
fn a_tab_may_separate_a_field_from_its_value() {
    // a tab is both valid separator whitespace and a control character. Measuring the end of the
    // value from the field name rather than from the value made these panic outright.
    assert_eq!(properties_get_field("$name:\tbay", "$name"), Some("bay"));
    assert_eq!(properties_get_field("$name\tbay", "$name"), Some("bay"));
    assert_eq!(properties_get_field("$name =\t bay\n$other=1", "$name"), Some("bay"));
}

#[test]
fn a_multi_byte_char_doesnt_throw_the_offsets_off() {
    // a char index is not a byte offset, and conflating the two panicked on any non ASCII property
    assert_eq!(properties_get_field("$name=café\n$other=1", "$name"), Some("café"));
    assert_eq!(properties_get_field("$name=café", "$name"), Some("café"));
    assert_eq!(properties_get_field("$name=ünïcödé bay\n$other=1", "$name"), Some("ünïcödé bay"));
    // a non breaking space is whitespace, so it separates - and it is two bytes wide
    assert_eq!(properties_get_field("$name\u{00A0}bay", "$name"), Some("bay"));
}

#[test]
fn a_field_with_no_value_reads_as_empty() {
    assert_eq!(properties_get_field("$name=", "$name"), Some(""));
    assert_eq!(properties_get_field("$name", "$name"), Some(""));
    assert_eq!(properties_get_field("$name=\n$other=1", "$name"), Some(""));
}

// ---------------------------------------------------------------- updating

#[test]
fn updating_replaces_only_the_value() {
    let mut props = "$name=Fighterbay\n$other=1".to_string();
    properties_update_field(&mut props, "$name", "Bomberbay");
    assert_eq!(props, "$name=Bomberbay\n$other=1");

    // this used to panic rather than update
    let mut props = "$name:\tFighterbay".to_string();
    properties_update_field(&mut props, "$name", "Bomberbay");
    assert_eq!(props, "$name:\tBomberbay");
    assert_eq!(properties_get_field(&props, "$name"), Some("Bomberbay"));

    let mut props = "$name=café\n$other=1".to_string();
    properties_update_field(&mut props, "$name", "bay");
    assert_eq!(props, "$name=bay\n$other=1");
}

#[test]
fn giving_a_valueless_field_a_value_writes_a_separator() {
    // "$special" on its own has no separator to put the value after, and writing one without it
    // produced "$specialsubsystem" - which FSO doesn't recognize as $special at all, and which
    // properties_get_field then reads straight back as "subsystem", so nothing looked wrong here
    let mut props = "$special\n$name=X".to_string();
    properties_update_field(&mut props, "$special", "subsystem");
    assert_eq!(props, "$special=subsystem\n$name=X");
    assert_eq!(properties_get_field(&props, "$name"), Some("X"), "and the rest of the line is untouched");

    let mut props = "$special".to_string();
    properties_update_field(&mut props, "$special", "subsystem");
    assert_eq!(props, "$special=subsystem");

    let mut props = "$name=bay\n$special".to_string();
    properties_update_field(&mut props, "$special", "subsystem");
    assert_eq!(props, "$name=bay\n$special=subsystem");

    // a flag being turned into a field is the same shape
    let mut props = "$no_rotate\n$name=bay".to_string();
    properties_update_field(&mut props, "$no_rotate", "yes");
    assert_eq!(props, "$no_rotate=yes\n$name=bay");

    // ...while a field which already has a separator keeps the one it was written with
    let mut props = "$special=\n$name=X".to_string();
    properties_update_field(&mut props, "$special", "subsystem");
    assert_eq!(props, "$special=subsystem\n$name=X");

    let mut props = "$special:\t".to_string();
    properties_update_field(&mut props, "$special", "subsystem");
    assert_eq!(props, "$special:\tsubsystem");
}

#[test]
fn a_valueless_field_given_a_value_becomes_a_subsystem() {
    // the user reachable route in: the Special Point type combo box writes $special through
    // properties_update_field, and is_subsystem reads it back
    let mut spcl = SpecialPoint { properties: "$special".to_string(), ..Default::default() };
    assert!(!spcl.is_subsystem());
    properties_update_field(&mut spcl.properties, "$special", "subsystem");
    assert!(spcl.is_subsystem(), "wrote {:?}", spcl.properties);
}

#[test]
fn updating_a_missing_field_appends_it() {
    let mut props = "$other=1".to_string();
    properties_update_field(&mut props, "$name", "bay");
    assert_eq!(props, "$other=1\n$name=bay");
    assert_eq!(properties_get_field(&props, "$name"), Some("bay"));

    let mut props = String::new();
    properties_update_field(&mut props, "$name", "bay");
    assert_eq!(props, "$name=bay");
}

// ---------------------------------------------------------------- deleting

#[test]
fn deleting_removes_the_field_and_its_value() {
    let mut props = "$name=Fighterbay\n$other=1".to_string();
    properties_delete_field(&mut props, "$name");
    assert_eq!(props, "$other=1");

    // this used to leave the orphaned value behind
    let mut props = "$name:\tFighterbay".to_string();
    properties_delete_field(&mut props, "$name");
    assert_eq!(props, "");

    // and this used to panic
    let mut props = "$name=café\n$other=1".to_string();
    properties_delete_field(&mut props, "$name");
    assert_eq!(props, "$other=1");
}

#[test]
fn deleting_a_field_in_the_middle_leaves_no_blank_line() {
    let mut props = "$first=1\n$name=bay\n$last=2".to_string();
    properties_delete_field(&mut props, "$name");
    assert_eq!(props, "$first=1\n$last=2");
    assert_eq!(properties_get_field(&props, "$first"), Some("1"));
    assert_eq!(properties_get_field(&props, "$last"), Some("2"));
}

#[test]
fn deleting_a_missing_field_changes_nothing() {
    let mut props = "$other=1".to_string();
    properties_delete_field(&mut props, "$name");
    assert_eq!(props, "$other=1");
}

// ---------------------------------------------------------------- flags

#[test]
fn flags_round_trip() {
    let mut props = "$name=bay".to_string();
    properties_set_flag(&mut props, "$no_rotate");
    assert_eq!(props, "$name=bay\n$no_rotate");

    // setting an existing flag is a no-op
    properties_set_flag(&mut props, "$no_rotate");
    assert_eq!(props, "$name=bay\n$no_rotate");

    properties_remove_flag(&mut props, "$no_rotate");
    assert_eq!(props, "$name=bay");
}

// ---------------------------------------------------------------- through the model

#[test]
fn a_tab_separated_subsystem_is_recognized() {
    let smodel = Submodel { properties: "$special:\tsubsystem".to_string(), ..Default::default() };
    assert!(smodel.is_subsystem());

    let dock = Dock { properties: "$name:\tFighterbay\n$parent_submodel=hull".to_string(), ..Default::default() };
    assert_eq!(dock.get_name(), Some("Fighterbay"));
    assert_eq!(dock.get_parent_smodel(), Some("hull"));
}
