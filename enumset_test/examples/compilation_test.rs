//! This is a simple test program used to test the code generated for various operations.
//! Not meant for automatic testing, just manual checks.

use enumset::{enum_set, EnumSet, EnumSetType};
use std::fmt::Debug;
use std::hint::black_box;

#[derive(EnumSetType, Debug)]
#[rustfmt::skip]
enum SmallEnum {
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z,
}

#[derive(EnumSetType, Debug)]
#[rustfmt::skip]
pub enum SparseEnum {
    A = 0xA, B = 20, C = 30, D = 40, E = 50, F = 60, G = 70, H = 80,
}

#[derive(EnumSetType, Debug)]
#[enumset(repr = "u128")]
#[rustfmt::skip]
pub enum LargeEnum {
    _00,  _01,  _02,  _03,  _04,  _05,  _06,  _07,
    _10,  _11,  _12,  _13,  _14,  _15,  _16,  _17,
    _20,  _21,  _22,  _23,  _24,  _25,  _26,  _27,
    _30,  _31,  _32,  _33,  _34,  _35,  _36,  _37,
    _40,  _41,  _42,  _43,  _44,  _45,  _46,  _47,
    _50,  _51,  _52,  _53,  _54,  _55,  _56,  _57,
    _60,  _61,  _62,  _63,  _64,  _65,  _66,  _67,
    _70,  _71,  _72,  _73,  _74,  _75,  _76,  _77,
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z,
}

#[derive(EnumSetType, Debug)]
#[rustfmt::skip]
pub enum ArrayEnum {
    _00,  _01,  _02,  _03,  _04,  _05,  _06,  _07,
    _10,  _11,  _12,  _13,  _14,  _15,  _16,  _17,
    _20,  _21,  _22,  _23,  _24,  _25,  _26,  _27,
    _30,  _31,  _32,  _33,  _34,  _35,  _36,  _37,
    _40,  _41,  _42,  _43,  _44,  _45,  _46,  _47,
    _50,  _51,  _52,  _53,  _54,  _55,  _56,  _57,
    _60,  _61,  _62,  _63,  _64,  _65,  _66,  _67,
    _70,  _71,  _72,  _73,  _74,  _75,  _76,  _77,
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z,
}

#[derive(EnumSetType, Debug)]
#[enumset(map = "compact")]
#[rustfmt::skip]
pub enum CompactEnum {
    A = 10, B = 22, C = 36, D = 24, E = 88, F = 61, G = 20, H = 1259,
}

#[derive(EnumSetType, Debug)]
#[enumset(map = "msb", repr = "u64")]
#[rustfmt::skip]
enum MsbEnum {
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z,
}

#[derive(EnumSetType, Debug)]
#[enumset(map = "msb", repr = "u64")]
#[rustfmt::skip]
enum MsbSparseEnum {
    A = 0xA, B = 15, C = 22, D = 42, E = 55, F, G, H,
}

#[derive(EnumSetType, Debug)]
#[enumset(map = "mask")]
#[rustfmt::skip]
pub enum MaskEnum {
    A = 0x1, B = 0x2, C = 0x4, D = 0x8, E = 0x10, F = 0x20, G = 0x40, H = 0x80,
}

#[derive(EnumSetType, Debug)]
#[enumset(map = "mask")]
#[rustfmt::skip]
pub enum MaskSparseEnum {
    A = 0x1, B = 0x2, C = 0x10, D = 0x20, E = 0x100, F = 0x200, G = 0x1000, H = 0x10000,
}

#[inline(never)]
fn test_print<T: Debug>(t: T) {
    print!("{t:?}, ");
}

macro_rules! gen_tests {
    ($ty:ident, $mod_name:ident, $test_fn:ident) => {
        #[allow(improper_ctypes_definitions)]
        mod $mod_name {
            use super::*;

            #[inline(never)]
            fn print(t: $ty) {
                test_print(black_box(t));
            }

            #[inline(never)]
            fn test_insert(a: EnumSet<$ty>, b: $ty) -> EnumSet<$ty> {
                a | b
            }

            #[inline(never)]
            fn test_dump(a: EnumSet<$ty>) {
                for v in a {
                    print(v);
                }
            }

            pub fn insert(a: EnumSet<$ty>, b: $ty) -> EnumSet<$ty> {
                black_box(test_insert(black_box(a), black_box(b)))
            }

            pub fn dump(a: EnumSet<$ty>) {
                test_dump(black_box(a))
            }
        }

        fn $test_fn() {
            println!(concat!("== ", stringify!($ty), " =="));
            println!("{:?}", $mod_name::insert(enum_set!($ty::A | $ty::C), $ty::E));
            println!("{:?}", $mod_name::insert(enum_set!($ty::A | $ty::D), $ty::F));
            println!("{:?}", $mod_name::insert(enum_set!($ty::B | $ty::D), $ty::F));
            $mod_name::dump(enum_set!($ty::A | $ty::C | $ty::E));
            println!();
            $mod_name::dump(enum_set!($ty::A | $ty::D | $ty::F));
            println!();
            $mod_name::dump(enum_set!($ty::B | $ty::D | $ty::F));
            println!();
            println!();
        }
    };
}

gen_tests!(SmallEnum, small_enum, test_small_enum);
gen_tests!(SparseEnum, sparse_enum, test_sparse_enum);
gen_tests!(LargeEnum, large_enum, test_large_enum);
gen_tests!(ArrayEnum, array_enum, test_array_enum);
gen_tests!(CompactEnum, compact_enum, test_compact_enum);
gen_tests!(MsbEnum, msb_enum, test_msb_enum);
gen_tests!(MsbSparseEnum, msb_sparse_enum, test_msb_sparse_enum);
gen_tests!(MaskEnum, mask_enum, test_mask_enum);
gen_tests!(MaskSparseEnum, mask_sparse_enum, test_mask_sparse_enum);

fn main() {
    test_small_enum();
    test_sparse_enum();
    test_large_enum();
    test_array_enum();
    test_compact_enum();
    test_msb_enum();
    test_msb_sparse_enum();
    test_mask_enum();
    test_mask_sparse_enum();
}
