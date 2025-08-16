//! This is a simple test program used to test the code generated for various operations.
//! Not meant for automatic testing, just manual checks.

use std::fmt::Debug;
use std::hint::black_box;
use enumset::{enum_set, EnumSet, EnumSetType};

#[derive(EnumSetType, Debug)]
enum SmallEnum {
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z,
}

#[derive(EnumSetType, Debug)]
pub enum SparseEnum {
    A = 0xA, B = 20, C = 30, D = 40, E = 50, F = 60, G = 70, H = 80,
}

#[derive(EnumSetType, Debug)]
#[enumset(repr = "u128")]
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
pub enum CompactEnum {
    A = 10, B = 22, C = 36, D = 24, E = 88, F = 61, G = 20, H = 1259,
}

#[derive(EnumSetType, Debug)]
#[enumset(map = "msb", repr = "u64")]
enum MsbEnum {
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z,
}

#[derive(EnumSetType, Debug)]
#[enumset(map = "msb", repr = "u64")]
enum MsbSparseEnum {
    A = 0xA, B = 15, C = 22, D = 42, E = 55, F, G, H,
}

#[inline(never)]
fn test_print<T: Debug>(t: T) {
    println!("{} {t:?}", std::any::type_name::<T>());
}

macro_rules! gen_tests {
    ($ty:ident, $mod_name:ident, $test_fn:ident) => {
        mod $mod_name {
            use super::*;

            #[inline(never)]
            pub fn test_insert(a: EnumSet<$ty>, b: $ty) -> EnumSet<$ty> {
                black_box(black_box(a) | black_box(b))
            }

            #[inline(never)]
            pub fn test_dump(a: EnumSet<$ty>) {
                for v in black_box(a) {
                    test_print(black_box(v));
                }
            }
        }

        fn $test_fn() {
            println!(concat!("== ", stringify!($ty), " =="));
            println!("{:?}", $mod_name::test_insert(enum_set!($ty::A | $ty::C), $ty::E));
            println!("{:?}", $mod_name::test_insert(enum_set!($ty::A | $ty::D), $ty::F));
            $mod_name::test_dump(enum_set!($ty::A | $ty::C | $ty::E));
            $mod_name::test_dump(enum_set!($ty::A | $ty::D | $ty::F));
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

fn main() {
    test_small_enum();
    test_sparse_enum();
    test_large_enum();
    test_array_enum();
    test_compact_enum();
    test_msb_enum();
    test_msb_sparse_enum();
}