#![cfg(feature = "test_serde")]
#![deny(warnings)]
#![allow(dead_code)]

use enumset::*;
use serde::{Serialize, Deserialize};

// Test resistance against shadowed types.
type Some = ();
type None = ();
type Result = ();

#[derive(Serialize, Deserialize, EnumSetType, Debug)]
#[enumset(serialize_repr = "list")]
#[rustfmt::skip]
pub enum ListEnum {
    A, B, C, D, E, F, G, H,
}

#[derive(Serialize, Deserialize, EnumSetType, Debug)]
#[enumset(serialize_repr = "map")]
#[rustfmt::skip]
pub enum MapEnum {
    A, B, C, D, E, F, G, H,
}

#[derive(EnumSetType, Debug)]
#[enumset(serialize_repr = "array")]
#[rustfmt::skip]
pub enum ArrayEnum {
    A, B, C, D, E, F, G, H,
}

#[derive(EnumSetType, Debug)]
#[rustfmt::skip]
pub enum LargeEnum {
    A, B, C, D, E=200, F, G, H,
}

#[derive(EnumSetType, Debug)]
#[enumset(serialize_repr = "u128")]
#[rustfmt::skip]
pub enum ReprEnum {
    A, B, C, D, E, F, G, H,
}

#[derive(EnumSetType, Debug)]
#[enumset(serialize_repr = "u128", serialize_deny_unknown)]
#[rustfmt::skip]
pub enum DenyUnknownEnum {
    A, B, C, D, E, F, G, H,
}

#[derive(EnumSetType, Debug)]
#[enumset(repr = "u64", serialize_repr = "u32")]
#[rustfmt::skip]
pub enum MixedEnum {
    A = 10, B, C, D, E, F, G, H,
}

#[derive(EnumSetType, Debug)]
#[enumset(serialize_repr = "array", serialize_deny_unknown)]
#[rustfmt::skip]
pub enum DenyUnknownEnumArray {
    A = 40, B, C, D, E, F, G, H,
}