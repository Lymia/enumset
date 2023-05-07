use enumset::*;

macro_rules! read_array {
    ($set:expr, $size:expr) => {{
        let mut arr = [0; $size];
        $set.copy_into_slice(&mut arr);
        arr
    }};
}

#[derive(EnumSetType, Debug)]
pub enum Enum8 {
    A, B, C, D, E, F, G,
    // H omitted for non-existent bit test
}

#[test]
fn conversions_enum_8() {
    // check conversion to integer
    assert_eq!(7, (Enum8::A | Enum8::B | Enum8::C).as_u8());
    assert_eq!(7, (Enum8::A | Enum8::B | Enum8::C).as_u16());
    assert_eq!(7, (Enum8::A | Enum8::B | Enum8::C).as_u32());
    assert_eq!(7, (Enum8::A | Enum8::B | Enum8::C).as_u64());
    assert_eq!(7, (Enum8::A | Enum8::B | Enum8::C).as_u128());

    // check conversion from integer
    assert_eq!(Some(Enum8::A | Enum8::B | Enum8::C), EnumSet::try_from_u8(7));
    assert_eq!(None, EnumSet::<Enum8>::try_from_u8(7 | (1 << 7)));
    assert_eq!(None, EnumSet::<Enum8>::try_from_u16(7 | (1 << 15)));
    assert_eq!(None, EnumSet::<Enum8>::try_from_u32(7 | (1 << 31)));
    assert_eq!(None, EnumSet::<Enum8>::try_from_u64(7 | (1 << 63)));
    assert_eq!(None, EnumSet::<Enum8>::try_from_u128(7 | (1 << 127)));

    // check truncated conversion from integer
    assert_eq!(Enum8::A | Enum8::B | Enum8::C, EnumSet::from_u8_truncated(7));
    assert_eq!(Enum8::A | Enum8::B | Enum8::C, EnumSet::from_u8_truncated(7 | (1 << 7)));
    assert_eq!(Enum8::A | Enum8::B | Enum8::C, EnumSet::from_u16_truncated(7 | (1 << 15)));
    assert_eq!(Enum8::A | Enum8::B | Enum8::C, EnumSet::from_u32_truncated(7 | (1 << 31)));
    assert_eq!(Enum8::A | Enum8::B | Enum8::C, EnumSet::from_u64_truncated(7 | (1 << 63)));
    assert_eq!(Enum8::A | Enum8::B | Enum8::C, EnumSet::from_u128_truncated(7 | (1 << 127)));

    // check conversion to array
    assert_eq!((Enum8::A | Enum8::B | Enum8::C).as_array_truncated(), []);
    assert_eq!(EnumSet::<Enum8>::EMPTY.as_array_truncated(), []);
    assert_eq!((Enum8::A | Enum8::B | Enum8::C).as_array(), [7]);
    assert_eq!((Enum8::A | Enum8::B | Enum8::C).as_array(), [7, 0]);
    assert_eq!((Enum8::A | Enum8::B | Enum8::C).as_array(), [7, 0, 0]);
    assert_eq!((Enum8::A | Enum8::B | Enum8::C).as_array(), [7, 0, 0, 0]);
    assert_eq!(read_array!(Enum8::A | Enum8::B | Enum8::C, 1), [7]);
    assert_eq!(read_array!(Enum8::A | Enum8::B | Enum8::C, 2), [7, 0]);
    assert_eq!(read_array!(Enum8::A | Enum8::B | Enum8::C, 3), [7, 0, 0]);
    assert_eq!(read_array!(Enum8::A | Enum8::B | Enum8::C, 4), [7, 0, 0, 0]);

    // TODO: Further tests
}

#[test]
#[should_panic]
fn conversions_enum_8_fail_8_invalid() {
    EnumSet::<Enum8>::from_u8(7 | (1 << 7));
}

#[test]
#[should_panic]
fn conversions_enum_8_fail_16() {
    EnumSet::<Enum8>::from_u16(7 | (1 << 8));
}

#[test]
#[should_panic]
fn conversions_enum_8_fail_32() {
    EnumSet::<Enum8>::from_u32(7 | (1 << 16));
}

#[test]
#[should_panic]
fn conversions_enum_8_fail_64() {
    EnumSet::<Enum8>::from_u64(7 | (1 << 32));
}

#[test]
#[should_panic]
fn conversions_enum_8_fail_128() {
    EnumSet::<Enum8>::from_u128(7 | (1 << 64));
}

#[test]
#[should_panic]
fn conversions_enum_8_fail_array() {
    (Enum8::A | Enum8::B | Enum8::C).as_array::<0>();
}

// TODO: Further tests

#[derive(EnumSetType, Debug)]
pub enum Enum16 {
    A, B, C, D, E=8, F, G, H,
}

// TODO: Further tests

#[derive(EnumSetType, Debug)]
pub enum Enum32 {
    A, B, C, D, E=16, F, G, H,
}

// TODO: Further tests

#[derive(EnumSetType, Debug)]
pub enum Enum64 {
    A, B, C, D, E=32, F, G, H,
}

// TODO: Further tests

#[derive(EnumSetType, Debug)]
pub enum Enum128 {
    A, B, C, D, E=64, F, G, H,
}

// TODO: Further tests

#[derive(EnumSetType, Debug)]
pub enum Enum192 {
    A, B, C, D, E=128, F, G, H,
}

// TODO: Further tests

#[derive(EnumSetType, Debug)]
pub enum Enum256 {
    A, B, C, D, E=192, F, G, H,
}

// TODO: Further tests

