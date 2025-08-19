use serde::*;
use enumset::EnumSetType;

// double check hygiene
enum Ok {}
enum None {}

#[derive(EnumSetType, Serialize, Deserialize)]
#[enumset(repr = "u8", map = "msb")]
#[rustfmt::skip]
enum TestU8 {
    A, B, C, D, E, F, G
}

#[derive(EnumSetType, Serialize, Deserialize)]
#[enumset(repr = "u16", map = "msb")]
#[rustfmt::skip]
enum TestU16 {
    A, B, C, D, E, F, G
}

#[derive(EnumSetType, Serialize, Deserialize)]
#[enumset(repr = "u32", map = "msb")]
#[rustfmt::skip]
enum TestU32 {
    A, B, C, D, E, F, G
}

#[derive(EnumSetType, Serialize, Deserialize)]
#[enumset(repr = "u64", map = "msb")]
#[rustfmt::skip]
enum TestU64 {
    A, B, C, D, E, F, G
}

#[derive(EnumSetType, Serialize, Deserialize)]
#[enumset(repr = "u128", map = "msb")]
#[rustfmt::skip]
enum TestU128 {
    A, B, C, D, E, F, G
}

fn main() {}