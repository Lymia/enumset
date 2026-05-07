use enumset::*;

#[derive(EnumSetType)]
#[enumset(map = "msb")]
enum MsbWithoutRepr {
    A, B, C, D
}

#[derive(EnumSetType)]
#[enumset(map = "msb", repr = "u64")]
enum MsbExcessSize {
    A, B, C, D = 64
}

#[derive(EnumSetType)]
#[enumset(map = "mask")]
enum MaskNotValid {
    A = 0x3,
}

#[derive(EnumSetType)]
#[enumset(map = "invalid_mask")]
enum MappingNotValid {
    A, B, C, D,
}

fn main() {}
