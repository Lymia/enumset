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

#[derive(EnumSetType, Debug)]
pub enum Enum16 {
    A, B, C, D, E=8, F, G, H,
}

#[derive(EnumSetType, Debug)]
pub enum Enum32 {
    A, B, C, D, E=16, F, G, H,
}

#[derive(EnumSetType, Debug)]
pub enum Enum64 {
    A, B, C, D, E=32, F, G, H,
}

#[derive(EnumSetType, Debug)]
pub enum Enum128 {
    A, B, C, D, E=64, F, G, H,
}

#[derive(EnumSetType, Debug)]
pub enum Enum192 {
    A, B, C, D, E=128, F, G, H,
}

#[derive(EnumSetType, Debug)]
pub enum Enum256 {
    A, B, C, D, E=192, F, G, H,
}

macro_rules! check_simple_conversion {
    ($mod:ident, $e:ident) => {
        mod $mod {
            use super::*;

            #[test]
            fn to_integer() {
                assert_eq!(7,
                           ($e::A | $e::B | $e::C).as_u8());
                assert_eq!(7,
                           ($e::A | $e::B | $e::C).as_u16());
                assert_eq!(7,
                           ($e::A | $e::B | $e::C).as_u32());
                assert_eq!(7,
                           ($e::A | $e::B | $e::C).as_u64());
                assert_eq!(7,
                           ($e::A | $e::B | $e::C).as_u128());
            }

            #[test]
            fn try_from_integer() {
                assert_eq!(Some($e::A | $e::B | $e::C),
                           EnumSet::try_from_u8(7));
                assert_eq!(None,
                           EnumSet::<$e>::try_from_u8(7 | (1 << 7)));
                assert_eq!(None,
                           EnumSet::<$e>::try_from_u16(7 | (1 << 15)));
                assert_eq!(None,
                           EnumSet::<$e>::try_from_u32(7 | (1 << 31)));
                assert_eq!(None,
                           EnumSet::<$e>::try_from_u64(7 | (1 << 63)));
                assert_eq!(None,
                           EnumSet::<$e>::try_from_u128(7 | (1 << 127)));
            }

            #[test]
            fn from_integer_truncated() {
                assert_eq!($e::A | $e::B | $e::C,
                           EnumSet::from_u8_truncated(7));
                assert_eq!($e::A | $e::B | $e::C,
                           EnumSet::from_u8_truncated(7 | (1 << 7)));
                assert_eq!($e::A | $e::B | $e::C,
                           EnumSet::from_u16_truncated(7 | (1 << 15)));
                assert_eq!($e::A | $e::B | $e::C,
                           EnumSet::from_u32_truncated(7 | (1 << 31)));
                assert_eq!($e::A | $e::B | $e::C,
                           EnumSet::from_u64_truncated(7 | (1 << 63)));
                assert_eq!($e::A | $e::B | $e::C,
                           EnumSet::from_u128_truncated(7 | (1 << 127)));
            }

            #[test]
            fn basic_to_array() {
                assert_eq!(($e::A | $e::B | $e::C).as_array_truncated(), 
                           []);
                assert_eq!(EnumSet::<$e>::EMPTY.as_array_truncated(), 
                           []);
                assert_eq!(($e::A | $e::B | $e::C).as_array(), 
                           [7]);
                assert_eq!(($e::A | $e::B | $e::C).as_array(), 
                           [7, 0]);
                assert_eq!(($e::A | $e::B | $e::C).as_array(), 
                           [7, 0, 0]);
                assert_eq!(($e::A | $e::B | $e::C).as_array(), 
                           [7, 0, 0, 0]);
                assert_eq!(read_array!($e::A | $e::B | $e::C, 1), 
                           [7]);
                assert_eq!(read_array!($e::A | $e::B | $e::C, 2), 
                           [7, 0]);
                assert_eq!(read_array!($e::A | $e::B | $e::C, 3), 
                           [7, 0, 0]);
                assert_eq!(read_array!($e::A | $e::B | $e::C, 4), 
                           [7, 0, 0, 0]);
            }

            // TODO: Further tests (array read)

            #[test]
            #[should_panic]
            fn fail_from_u8() {
                EnumSet::<$e>::from_u8(7 | (1 << 7));
            }

            #[test]
            #[should_panic]
            fn fail_from_u16() {
                EnumSet::<$e>::from_u16(7 | (1 << 15));
            }

            #[test]
            #[should_panic]
            fn fail_from_u32() {
                EnumSet::<$e>::from_u32(7 | (1 << 31));
            }

            #[test]
            #[should_panic]
            fn fail_from_u64() {
                EnumSet::<$e>::from_u64(7 | (1 << 63));
            }

            #[test]
            #[should_panic]
            fn fail_from_u128() {
                EnumSet::<$e>::from_u128(7 | (1 << 127));
            }

            #[test]
            #[should_panic]
            fn fail_to_array() {
                ($e::A | $e::B | $e::C).as_array::<0>();
            }

            // TODO: Further tests (array read)
        }
    };
}

check_simple_conversion!(enum_8_simple, Enum8);
check_simple_conversion!(enum_16_simple, Enum16);
check_simple_conversion!(enum_32_simple, Enum32);
check_simple_conversion!(enum_64_simple, Enum64);
check_simple_conversion!(enum_128_simple, Enum128);
check_simple_conversion!(enum_192_simple, Enum192);
check_simple_conversion!(enum_256_simple, Enum256);