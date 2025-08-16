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

fn main() {}
