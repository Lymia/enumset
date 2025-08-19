use serde::*;
use enumset::EnumSetType;

// double check hygiene
enum Ok {}
enum None {}

#[derive(EnumSetType, Serialize, Deserialize)]
#[enumset(repr = "u32", serialize_repr = "array")]
#[rustfmt::skip]
enum Test1 {
    A, B, C, D, E, F, G
}

#[derive(EnumSetType, Serialize, Deserialize)]
#[enumset(repr = "array", serialize_repr = "u32")]
#[rustfmt::skip]
enum Test2 {
    A, B, C, D, E, F, G
}

#[derive(EnumSetType, Serialize, Deserialize)]
#[enumset(repr = "u32", serialize_repr = "u32")]
#[rustfmt::skip]
enum Test3 {
    A, B, C, D, E, F, G
}

#[derive(EnumSetType, Serialize, Deserialize)]
#[enumset(repr = "array", serialize_repr = "array")]
#[rustfmt::skip]
enum Test4 {
    A, B, C, D, E, F, G
}

#[derive(EnumSetType, Serialize, Deserialize)]
#[enumset(repr = "u128", serialize_repr = "array")]
#[rustfmt::skip]
enum Test5 {
    A, B, C, D, E, F, G
}

#[derive(EnumSetType, Serialize, Deserialize)]
#[enumset(repr = "array", serialize_repr = "u128")]
#[rustfmt::skip]
enum Test6 {
    A, B, C, D, E, F, G
}

#[derive(EnumSetType, Serialize, Deserialize)]
#[enumset(repr = "u128", serialize_repr = "u128")]
#[rustfmt::skip]
enum Test7 {
    A, B, C, D, E, F, G
}

#[derive(EnumSetType, Serialize, Deserialize)]
#[enumset(repr = "u32", serialize_repr = "array", serialize_deny_unknown)]
#[rustfmt::skip]
enum Test8 {
    A, B, C, D, E, F, G
}

#[derive(EnumSetType, Serialize, Deserialize)]
#[enumset(repr = "array", serialize_repr = "u32", serialize_deny_unknown)]
#[rustfmt::skip]
enum Test9 {
    A, B, C, D, E, F, G
}

#[derive(EnumSetType, Serialize, Deserialize)]
#[enumset(repr = "u32", serialize_repr = "list", serialize_deny_unknown)]
#[rustfmt::skip]
enum Test10 {
    A, B, C, D, E, F, G
}

#[derive(EnumSetType, Serialize, Deserialize)]
#[enumset(repr = "array", serialize_repr = "list", serialize_deny_unknown)]
#[rustfmt::skip]
enum Test11 {
    A, B, C, D, E, F, G
}

#[derive(EnumSetType, Serialize, Deserialize)]
#[enumset(repr = "u32", serialize_repr = "map", serialize_deny_unknown)]
#[rustfmt::skip]
enum Test12 {
    A, B, C, D, E, F, G
}

#[derive(EnumSetType, Serialize, Deserialize)]
#[enumset(repr = "array", serialize_repr = "map", serialize_deny_unknown)]
#[rustfmt::skip]
enum Test13 {
    A, B, C, D, E, F, G
}

#[derive(EnumSetType, Serialize, Deserialize)]
#[enumset(repr = "u32", serialize_repr = "list")]
#[rustfmt::skip]
enum Test14 {
    A, B, C, D, E, F, G
}

#[derive(EnumSetType, Serialize, Deserialize)]
#[enumset(repr = "array", serialize_repr = "list")]
#[rustfmt::skip]
enum Test15 {
    A, B, C, D, E, F, G
}

#[derive(EnumSetType, Serialize, Deserialize)]
#[enumset(repr = "u32", serialize_repr = "map")]
#[rustfmt::skip]
enum Test16 {
    A, B, C, D, E, F, G
}

#[derive(EnumSetType, Serialize, Deserialize)]
#[enumset(repr = "array", serialize_repr = "map")]
#[rustfmt::skip]
enum Test17 {
    A, B, C, D, E, F, G
}

fn main() {}