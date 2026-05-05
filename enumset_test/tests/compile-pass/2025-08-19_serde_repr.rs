use serde::*;
use enumset::EnumSetType;

// double check hygiene
enum Ok {}
enum None {}

#[derive(EnumSetType, Serialize, Deserialize)]
#[enumset(serialize_repr = "u8")]
#[rustfmt::skip]
enum TestU8 {
    A, B, C, D, E, F, G
}

#[derive(EnumSetType, Serialize, Deserialize)]
#[enumset(serialize_repr = "u16")]
#[rustfmt::skip]
enum TestU16 {
    A, B, C, D, E, F, G
}

#[derive(EnumSetType, Serialize, Deserialize)]
#[enumset(serialize_repr = "u32")]
#[rustfmt::skip]
enum TestU32 {
    A, B, C, D, E, F, G
}

#[derive(EnumSetType, Serialize, Deserialize)]
#[enumset(serialize_repr = "u64")]
#[rustfmt::skip]
enum TestU64 {
    A, B, C, D, E, F, G
}

#[derive(EnumSetType, Serialize, Deserialize)]
#[enumset(serialize_repr = "u128")]
#[rustfmt::skip]
enum TestU128 {
    A, B, C, D, E, F, G
}

fn main() {}