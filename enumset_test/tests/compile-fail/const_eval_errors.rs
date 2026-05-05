use enumset::*;

#[derive(EnumSetType)]
enum BadVariants {
    Variant = -(const { 1 + 1 }),
}

#[derive(EnumSetType)]
enum BadVariants2 {
    Variant = !(1 > 2),
}

#[derive(EnumSetType)]
#[repr(u64)]
enum BadVariants3 {
    Variant = (1u64 << 0x100000000u64) + 1,
}

#[derive(EnumSetType)]
#[repr(u64)]
enum BadVariants4 {
    Variant = 1 + (1u64 >> 0x100000000u64),
}

#[derive(EnumSetType)]
enum BadVariants5 {
    Variant = "test",
}

#[derive(EnumSetType)]
enum BadVariants6 {
    Variant = *(&5),
}

#[derive(EnumSetType)]
#[repr(u128)]
enum BadVariants7 {
    Variant = 0x10000000000000000u128,
}

fn main() {}