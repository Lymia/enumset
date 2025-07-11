#![allow(dead_code)]
#![deny(warnings)]

use enumset::*;
use std::collections::{HashSet, BTreeSet};
use std::fmt::{Debug, Display, Formatter};

/// Used to test the degenerate case of an uninhabited enum.
#[derive(EnumSetType, Debug)]
pub enum EmptyEnum { }

// Test of a relatively typical enum.
#[derive(EnumSetType, Debug)]
enum SmallEnum {
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z,
}

// Used to test enums where the required impls are derived manually.
#[derive(Clone, Copy, Debug, EnumSetType, Eq, PartialEq)]
#[enumset(no_super_impls)]
enum SmallEnumExplicitDerive {
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z,
}

// Used to test very large enums.
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

/// Used to test enums with sparse elements.
#[derive(EnumSetType, Debug)]
pub enum SparseEnum {
    A = 0xA, B = 20, C = 30, D = 40, E = 50, F = 60, G = 70, H = 80,
}

/// Used to test the edge case of an enum with a single element.
#[derive(EnumSetType, Debug)]
pub enum Enum1 {
    A,
}

/// Used to test the edge case of an enum with eight variants (exactly fits in an u8)
#[derive(EnumSetType, Debug)]
pub enum Enum8 {
    A, B, C, D, E, F, G, H,
}

/// Used to test the edge case of an enum with 128 variants (exactly fits in an u128)
#[derive(EnumSetType, Debug)]
pub enum Enum128 {
    A, B, C, D, E, F, G, H, _8, _9, _10, _11, _12, _13, _14, _15,
    _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31,
    _32, _33, _34, _35, _36, _37, _38, _39, _40, _41, _42, _43, _44, _45, _46, _47,
    _48, _49, _50, _51, _52, _53, _54, _55, _56, _57, _58, _59, _60, _61, _62, _63,
    _64, _65, _66, _67, _68, _69, _70, _71, _72, _73, _74, _75, _76, _77, _78, _79,
    _80, _81, _82, _83, _84, _85, _86, _87, _88, _89, _90, _91, _92, _93, _94, _95,
    _96, _97, _98, _99, _100, _101, _102, _103, _104, _105, _106, _107, _108, _109,
    _110, _111, _112, _113, _114, _115, _116, _117, _118, _119, _120, _121, _122,
    _123, _124,  _125, _126, _127,
}

/// Used to test repr on enums.
#[repr(u32)]
#[derive(EnumSetType, Debug)]
pub enum ReprEnum {
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z,
}

/// Used to test repr on enums. (cont)
#[repr(u64)]
#[derive(EnumSetType, Debug)]
pub enum ReprEnum2 {
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z,
}

/// Used to test repr on enums. (cont)
#[repr(isize)]
#[derive(EnumSetType, Debug)]
pub enum ReprEnum3 {
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z,
}

/// Used to test repr on enums. (cont)
#[repr(C)]
#[derive(EnumSetType, Debug)]
pub enum ReprEnum4 {
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z,
}

/// Used to test massive enums that require arrays to use.
#[derive(EnumSetType, Debug)]
pub enum GiantEnum {
    A = 100, B = 200, C = 300, D = 400, E = 500, F = 600, G = 700, H = 800,
}

/// Used to test enums that are explicitly forced to use the array representation.
#[derive(EnumSetType, Debug)]
#[enumset(repr = "array")]
pub enum SmallArrayEnum {
    A, B, C, D, E, F, G, H
}

/// Used to test the edge case of an enum exactly one bit larger than a single u64 word
#[derive(EnumSetType, Debug)]
#[enumset(repr = "array")]
pub enum MarginalArrayEnumS2 {
    A, B, C, D, E, F, G, H, Marginal = 64,
}

/// Used to test the edge case of an enum that requires two u64 words.
#[derive(EnumSetType, Debug)]
#[enumset(repr = "array")]
pub enum MarginalArrayEnumS2H {
    A = 64, B, C, D, E, F, G, H, Marginal = 127,
}

/// Used to test the edge case of an enum that requires one more bit than two u64 words.
#[derive(EnumSetType, Debug)]
#[enumset(repr = "array")]
pub enum MarginalArrayEnumS3 {
    A, B, C, D, E, F, G, H, Marginal = 128,
}

/// Tests that all variants are properly present when `EnumSet::all` is used.
macro_rules! test_variants {
    ($enum_name:ident $all_empty_test:ident $($variant:ident,)*) => {
        #[test]
        fn $all_empty_test() {
            let all = EnumSet::<$enum_name>::all();
            let empty = EnumSet::<$enum_name>::empty();

            $(
                assert!(!empty.contains($enum_name::$variant));
                assert!(all.contains($enum_name::$variant));
            )*
        }
    }
}
test_variants! { SmallEnum small_enum_all_empty
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z,
}
test_variants! { SmallEnumExplicitDerive small_enum_explicit_derive_all_empty
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z,
}
test_variants! { LargeEnum large_enum_all_empty
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
test_variants! { SparseEnum sparse_enum_all_empty
    A, B, C, D, E, F, G,
}

macro_rules! test_enum {
    ($e:ident, $mem_size:expr) => {
        const CONST_SET: EnumSet<$e> = enum_set!($e::A | $e::C);

        /// Tests that sets created at compiletime using const exprs work as expected.
        #[test]
        fn const_set() {
            const CONST_1_SET: EnumSet<$e> = enum_set!($e::A);
            const CONST_SET_CHAIN: EnumSet<$e> = enum_set!(CONST_SET | $e::D);
            const CONST_SET_CHAIN_2: EnumSet<$e> = enum_set!($e::E | CONST_SET);
            const CONST_SET_CHAIN_3: EnumSet<$e> = enum_set!(CONST_SET_CHAIN | CONST_SET_CHAIN_2);
            const CONST_UNION: EnumSet<$e> = enum_set_union!(CONST_SET_CHAIN, CONST_SET_CHAIN_2);
            const CONST_INTERSECTION: EnumSet<$e> = enum_set_intersection!(CONST_SET_CHAIN, CONST_SET_CHAIN_2);
            const CONST_DIFFERENCE: EnumSet<$e> = enum_set_difference!(CONST_UNION, CONST_SET);
            const CONST_COMPLEMENT: EnumSet<$e> = enum_set_complement!(CONST_SET);
            const CONST_SYMMETRIC_DIFFERENCE: EnumSet<$e> = enum_set_symmetric_difference!(CONST_SET_CHAIN, CONST_SET_CHAIN_2);
            const EMPTY_SET: EnumSet<$e> = EnumSet::empty();
            const ALL_SET: EnumSet<$e> = EnumSet::all();
            const VARIANT_COUNT: usize = EnumSet::<$e>::variant_count() as usize;

            assert_eq!(CONST_SET, $e::A | $e::C);
            assert_eq!(CONST_1_SET, $e::A);
            assert_eq!(CONST_SET_CHAIN, $e::A | $e::C | $e::D);
            assert_eq!(CONST_SET_CHAIN_2, $e::A | $e::C | $e::E);
            assert_eq!(CONST_SET_CHAIN_3, $e::A | $e::C | $e::D | $e::E);
            assert_eq!(CONST_UNION, $e::A | $e::C | $e::D | $e::E);
            assert_eq!(CONST_INTERSECTION, $e::A | $e::C);
            assert_eq!(CONST_COMPLEMENT, !($e::A | $e::C));
            assert_eq!(CONST_SYMMETRIC_DIFFERENCE, $e::D | $e::E);

            assert!(EMPTY_SET.is_empty());
            assert_eq!(ALL_SET.len(), VARIANT_COUNT);
            assert_eq!(ALL_SET, EnumSet::all());
        }

        /// Tests that enumsets work in match expressions.
        #[test]
        fn match_const_test() {
            match CONST_SET {
                CONST_SET => { /* ok */ }
                _ => panic!("match fell through?"),
            }
        }

        // Basic tests to ensure insert/remove operations work properly.
        #[test]
        fn basic_add_remove() {
            let mut set = EnumSet::new();
            set.insert($e::A);
            set.insert($e::B);
            set.insert($e::C);
            assert_eq!(set, $e::A | $e::B | $e::C);
            set.remove($e::B);
            assert_eq!(set, $e::A | $e::C);
            set.insert($e::D);
            assert_eq!(set, $e::A | $e::C | $e::D);
            set.insert_all($e::F | $e::E | $e::G);
            assert_eq!(set, $e::A | $e::C | $e::D | $e::F | $e::E | $e::G);
            set.remove_all($e::A | $e::D | $e::G);
            assert_eq!(set, $e::C | $e::F | $e::E);
            assert!(!set.is_empty());
            set.clear();
            assert!(set.is_empty());
        }

        /// Basic tests to ensure the return status for `insert` is correct.
        #[test]
        fn already_present_element() {
            let mut set = EnumSet::new();
            assert!(set.insert($e::A));
            assert!(!set.insert($e::A));
            set.remove($e::A);
            assert!(set.insert($e::A));
        }

        /// Basic test to ensure `empty` actually returns an empty set.
        #[test]
        fn empty_is_empty() {
            assert_eq!(EnumSet::<$e>::empty().len(), 0)
        }

        /// Basic test to ensure that the length of the complete set is as expected.
        #[test]
        fn all_len() {
            assert_eq!(EnumSet::<$e>::all().len(), EnumSet::<$e>::variant_count() as usize)
        }

        /// Tests the basic functionality of the iterator tyep.
        #[test]
        fn iter_basic_ops() {
            let mut set = EnumSet::new();
            set.insert($e::A);
            set.insert($e::B);
            set.extend($e::C | $e::E);

            let mut set_2 = EnumSet::new();
            let vec: Vec<_> = set.iter().collect();
            for val in vec {
                assert!(!set_2.contains(val));
                set_2.insert(val);
            }
            assert_eq!(set, set_2);

            let mut set_3 = EnumSet::new();
            for val in set {
                assert!(!set_3.contains(val));
                set_3.insert(val);
            }
            assert_eq!(set, set_3);

            let mut set_4 = EnumSet::new();
            let vec: EnumSet<_> = set.into_iter().map(EnumSet::only).collect();
            for val in vec {
                assert!(!set_4.contains(val));
                set_4.insert(val);
            }
            assert_eq!(set, set_4);

            let mut set_5 = EnumSet::new();
            let vec: EnumSet<_> = set.iter().collect();
            for val in vec {
                assert!(!set_5.contains(val));
                set_5.insert(val);
            }
            assert_eq!(set, set_5);
        }

        /// Tests that iteration on an empty seti n fact returns nothing.
        #[test]
        fn empty_iter() {
            for _ in EnumSet::<$e>::new() {
                panic!("should not happen");
            }
        }

        /// Tests that the iterator returns enums in the expected order.
        #[test]
        fn iter_ordering() {
            let set_a = $e::A | $e::B | $e::E;
            let vec_a: Vec<_> = set_a.iter().collect();
            assert_eq!(vec_a, &[$e::A, $e::B, $e::E]);
            let vec_a_rev: Vec<_> = set_a.iter().rev().collect();
            assert_eq!(vec_a_rev, &[$e::E, $e::B, $e::A]);

            let set_b = $e::B | $e::C | $e::D | $e::G;
            let vec_b: Vec<_> = set_b.iter().collect();
            assert_eq!(vec_b, &[$e::B, $e::C, $e::D, $e::G]);
            let vec_b_rev: Vec<_> = set_b.iter().rev().collect();
            assert_eq!(vec_b_rev, &[$e::G, $e::D, $e::C, $e::B]);
        }

        /// Tests that the size hint returned by the iterator is correct.
        #[test]
        fn iter_size_hint() {
            fn check_iter_size_hint(set: EnumSet<$e>) {
                let count = set.len();

                // check for forward iteration
                {
                    let mut itr = set.iter();
                    for idx in 0 .. count {
                        assert_eq!(itr.size_hint(), (count-idx, Some(count-idx)));
                        assert_eq!(itr.len(), count-idx);
                        assert!(itr.next().is_some());
                    }
                    assert_eq!(itr.size_hint(), (0, Some(0)));
                    assert_eq!(itr.len(), 0);
                }

                // check for backwards iteration
                {
                    let mut itr = set.iter().rev();
                    for idx in 0 .. count {
                        assert_eq!(itr.size_hint(), (count-idx, Some(count-idx)));
                        assert_eq!(itr.len(), count-idx);
                        assert!(itr.next().is_some());
                    }
                    assert_eq!(itr.size_hint(), (0, Some(0)));
                    assert_eq!(itr.len(), 0);
                }
            }

            check_iter_size_hint(EnumSet::<$e>::new());
            check_iter_size_hint(EnumSet::<$e>::all());
            let mut set = EnumSet::new();
            set.insert($e::A);
            set.insert($e::C);
            set.insert($e::E);
            check_iter_size_hint(set);
        }

        /// Check that advanced iterator operations like filter and collect work properly.
        #[test]
        fn iter_advanced_ops_test() {
            let set = $e::A | $e::B | $e::C | $e::E;
            let set2 = set.iter().filter(|&v| v != $e::B).collect::<EnumSet<_>>();
            assert_eq!(set2, $e::A | $e::C | $e::E);
        }

        /// Basic test for arithmetic operators on bitsets.
        #[test]
        fn basic_ops_test() {
            assert_eq!(($e::A | $e::B) | ($e::B | $e::C), $e::A | $e::B | $e::C);
            assert_eq!(($e::A | $e::B) & ($e::B | $e::C), $e::B);
            assert_eq!(($e::A | $e::B) ^ ($e::B | $e::C), $e::A | $e::C);
            assert_eq!(($e::A | $e::B) - ($e::B | $e::C), $e::A);
            assert_eq!($e::A | !$e::A, EnumSet::<$e>::all());
        }

        /// Basic test for mutate-in-place operators on bitsets.
        #[test]
        fn mutable_ops_test() {
            let mut set = $e::A | $e::B;
            assert_eq!(set, $e::A | $e::B);
            set |= $e::C | $e::D;
            assert_eq!(set, $e::A | $e::B | $e::C | $e::D);
            set -= $e::C;
            assert_eq!(set, $e::A | $e::B | $e::D);
            set ^= $e::B | $e::E;
            assert_eq!(set, $e::A | $e::D | $e::E);
            set &= $e::A | $e::E | $e::F;
            assert_eq!(set, $e::A | $e::E);
        }

        /// Basic test for disjoint/subset checking.
        #[test]
        fn test_disjoint_subset() {
            assert!(($e::A | $e::B | $e::C).is_disjoint($e::D | $e::E | $e::F));
            assert!(!($e::A | $e::B | $e::C | $e::D).is_disjoint($e::D | $e::E | $e::F));
            assert!(($e::A | $e::B).is_subset($e::A | $e::B | $e::C));
            assert!(!($e::A | $e::D).is_subset($e::A | $e::B | $e::C));
        }

        /// Basic test that `Debug` returns the expected outupt.
        #[test]
        fn debug_impl_has_expected_output() {
            assert_eq!(format!("{:?}", $e::A | $e::B | $e::D), "EnumSet(A | B | D)");
        }

        /// Basic test that converting to and from an integer works as expected.
        #[test]
        fn to_from_roundtrip() {
            let value = $e::A | $e::C | $e::D | $e::F | $e::E | $e::G;
            if EnumSet::<$e>::bit_width() < 128 {
                assert_eq!(EnumSet::from_u128(value.as_u128()), value);
            }
            if EnumSet::<$e>::bit_width() < 64 {
                assert_eq!(EnumSet::from_u64(value.as_u64()), value);
            }
            if EnumSet::<$e>::bit_width() < 32 {
                assert_eq!(EnumSet::from_u32(value.as_u32()), value);
            }
            if EnumSet::<$e>::bit_width() < 16 {
                assert_eq!(EnumSet::from_u16(value.as_u16()), value);
            }
            if EnumSet::<$e>::bit_width() < 8 {
                assert_eq!(EnumSet::from_u8(value.as_u8()), value);
            }
        }

        /// Tests that EnumSet correctly detects conversions containing invalid bits
        #[test]
        #[should_panic]
        fn should_panic_on_invalid_bits() {
            if EnumSet::<$e>::variant_count() == 128 {
                panic!("(test skipped)")
            }
            EnumSet::<$e>::from_u128(!0);
        }

        /// An advanced test to ensure that sets of enumsets work correctly.
        #[test]
        fn test_enumset_in_std_sets() {
            const SET_TEST_A: EnumSet<$e> = enum_set!($e::A | $e::B | $e::C);
            const SET_TEST_B: EnumSet<$e> = enum_set!($e::A | $e::B | $e::D);
            const SET_TEST_C: EnumSet<$e> = enum_set!($e::A | $e::B | $e::E);
            const SET_TEST_D: EnumSet<$e> = enum_set!($e::A | $e::B | $e::F);
            const SET_TEST_E: EnumSet<$e> = enum_set!($e::A | $e::B | $e::G);
            macro_rules! test_set {
                ($set:ident) => {{
                    assert!(!$set.contains(&SET_TEST_A));
                    assert!(!$set.contains(&SET_TEST_B));
                    assert!(!$set.contains(&SET_TEST_C));
                    assert!(!$set.contains(&SET_TEST_D));
                    assert!(!$set.contains(&SET_TEST_E));
                    $set.insert(SET_TEST_A);
                    $set.insert(SET_TEST_C);
                    assert!($set.contains(&SET_TEST_A));
                    assert!(!$set.contains(&SET_TEST_B));
                    assert!($set.contains(&SET_TEST_C));
                    assert!(!$set.contains(&SET_TEST_D));
                    assert!(!$set.contains(&SET_TEST_E));
                    $set.remove(&SET_TEST_C);
                    $set.remove(&SET_TEST_D);
                    assert!($set.contains(&SET_TEST_A));
                    assert!(!$set.contains(&SET_TEST_B));
                    assert!(!$set.contains(&SET_TEST_C));
                    assert!(!$set.contains(&SET_TEST_D));
                    assert!(!$set.contains(&SET_TEST_E));
                    $set.insert(SET_TEST_A);
                    $set.insert(SET_TEST_D);
                    assert!($set.contains(&SET_TEST_A));
                    assert!(!$set.contains(&SET_TEST_B));
                    assert!(!$set.contains(&SET_TEST_C));
                    assert!($set.contains(&SET_TEST_D));
                    assert!(!$set.contains(&SET_TEST_E));
                }}
            }

            let mut hash_set = HashSet::new();
            test_set!(hash_set);

            let mut tree_set = BTreeSet::new();
            test_set!(tree_set);
        }

        /// An advanced test that summing enumsets in iterators.
        #[test]
        fn sum_iterator_of_enumsets() {
            let target = $e::A | $e::B | $e::D | $e::E | $e::G | $e::H;

            let list_a = [$e::A | $e::B, $e::D | $e::E, $e::G | $e::H];
            let sum_a: EnumSet<$e> = list_a.iter().map(|x| *x).sum();
            assert_eq!(target, sum_a);
            let sum_b: EnumSet<$e> = list_a.iter().sum();
            assert_eq!(target, sum_b);

            let list_b = [$e::A, $e::B, $e::D, $e::E, $e::G, $e::H];
            let sum_c: EnumSet<$e> = list_b.iter().map(|x| *x).sum();
            assert_eq!(target, sum_c);
            let sum_d: EnumSet<$e> = list_b.iter().sum();
            assert_eq!(target, sum_d);
        }

        /// Checks that the enumset has the expected size in memory.
        #[test]
        fn check_size() {
            assert_eq!(::std::mem::size_of::<EnumSet<$e>>(), $mem_size);
        }

        /// Tests the conversion of const-length arrays to EnumSets.
        #[test]
        fn from_array() {
            assert!(EnumSet::<$e>::from([]).is_empty());
            assert_eq!(
                EnumSet::<$e>::from([$e::B, $e::E, $e::H]),
                enum_set!($e::B | $e::E | $e::H)
            );
            assert_eq!(
                EnumSet::<$e>::from([$e::A, $e::B, $e::C]),
                enum_set!($e::A | $e::B | $e::C)
            );
        }
    }
}
macro_rules! tests {
    ($m:ident, $($tt:tt)*) => { mod $m { use super::*; $($tt)*; } }
}

tests!(small_enum, test_enum!(SmallEnum, 4));
tests!(small_enum_explicit_derive, test_enum!(SmallEnumExplicitDerive, 4));
tests!(large_enum, test_enum!(LargeEnum, 16));
tests!(enum8, test_enum!(Enum8, 1));
tests!(enum128, test_enum!(Enum128, 16));
tests!(sparse_enum, test_enum!(SparseEnum, 16));
tests!(repr_enum_u32, test_enum!(ReprEnum, 4));
tests!(repr_enum_u64, test_enum!(ReprEnum2, 4));
tests!(repr_enum_isize, test_enum!(ReprEnum3, 4));
tests!(repr_enum_c, test_enum!(ReprEnum4, 4));
tests!(giant_enum, test_enum!(GiantEnum, 104));
tests!(small_array_enum, test_enum!(SmallArrayEnum, 8));
tests!(marginal_array_enum_s2, test_enum!(MarginalArrayEnumS2, 16));
tests!(marginal_array_enum_s2h, test_enum!(MarginalArrayEnumS2H, 16));
tests!(marginal_array_enum_s3, test_enum!(MarginalArrayEnumS3, 24));

#[derive(EnumSetType, Debug)]
pub enum ThresholdEnum {
    A = 1, B, C, D,
    U8 = 0, U16 = 8, U32 = 16, U64 = 32, U128 = 64,
}
macro_rules! bits_tests {
    (
        $mod_name:ident, $threshold_expr:expr, ($($too_big_expr:expr),*), $ty:ty,
        $to:ident $try_to:ident $to_truncated:ident
        $from:ident $try_from:ident $from_truncated:ident
    ) => {
        mod $mod_name {
            use super::*;
            use crate::ThresholdEnum::*;

            #[test]
            fn to_from_basic() {
                for &mask in &[
                    $threshold_expr | B | C | D,
                    $threshold_expr | A | D,
                    $threshold_expr | B | C,
                ] {
                    assert_eq!(mask, EnumSet::<ThresholdEnum>::$from(mask.$to()));
                    assert_eq!(mask.$to_truncated(), mask.$to());
                    assert_eq!(Some(mask.$to()), mask.$try_to())
                }
            }

            #[test]
            #[should_panic]
            fn from_invalid() {
                let invalid_mask: $ty = 0x80;
                EnumSet::<ThresholdEnum>::$from(invalid_mask);
            }

            #[test]
            fn try_from_invalid() {
                assert!(EnumSet::<ThresholdEnum>::$try_from(0xFF).is_none());
            }

            $(
                #[test]
                fn try_to_overflow() {
                        let set: EnumSet<ThresholdEnum> = $too_big_expr.into();
                        assert!(set.$try_to().is_none());
                }
            )*

            #[test]
            fn truncated_overflow() {
                let trunc_invalid = EnumSet::<ThresholdEnum>::$from_truncated(0xFE);
                assert_eq!(A | B | C | D, trunc_invalid);
                $(
                    let set: EnumSet<ThresholdEnum> = $too_big_expr | A;
                    assert_eq!(2, set.$to_truncated());
                )*
            }
        }
    }
}

bits_tests!(test_u8_bits, U8, (U16), u8,
            as_u8 try_as_u8 as_u8_truncated from_u8 try_from_u8 from_u8_truncated);
bits_tests!(test_u16_bits, U16, (U32), u16,
            as_u16 try_as_u16 as_u16_truncated from_u16 try_from_u16 from_u16_truncated);
bits_tests!(test_u32_bits, U32, (U64), u32,
            as_u32 try_as_u32 as_u32_truncated from_u32 try_from_u32 from_u32_truncated);
bits_tests!(test_u64_bits, U64, (U128), u64,
            as_u64 try_as_u64 as_u64_truncated from_u64 try_from_u64 from_u64_truncated);
bits_tests!(test_u128_bits, U128, (), u128,
            as_u128 try_as_u128 as_u128_truncated from_u128 try_from_u128 from_u128_truncated);
bits_tests!(test_usize_bits, U32, (U128), usize,
            as_usize try_as_usize as_usize_truncated
            from_usize try_from_usize from_usize_truncated);

impl Display for Enum8 {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}
#[test]
fn test_display_impl() {
    assert_eq!((Enum8::A | Enum8::D | Enum8::H).to_string(), "A | D | H");
}

mod tests_without_imports {
    // This deliberately does not use super::* or crate::*

    // The constants below should compile despite not having EnumSet or enumset
    // in scope.

    const EMPTY: crate::EnumSet<super::SmallEnum> = crate::enum_set!();
    const CONST_SET: crate::EnumSet<super::SmallEnum> = crate::enum_set!(super::SmallEnum::A | super::SmallEnum::C);
    const CONST_1_SET: crate::EnumSet<super::SmallEnum> = crate::enum_set!(super::SmallEnum::A);
}
