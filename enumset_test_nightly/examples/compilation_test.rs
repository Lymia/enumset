use std::fmt::Debug;
use std::hint::black_box;
use enumset::{enum_set, EnumSet, EnumSetType};

// Test of a relatively typical enum.
#[derive(EnumSetType, Debug)]
enum SmallEnum {
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z,
}

/// Used to test enums with sparse elements.
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

#[inline(never)]
fn test_insert1(a: EnumSet<SmallEnum>, b: SmallEnum) -> EnumSet<SmallEnum> {
    black_box(black_box(a) | black_box(b))
}

#[inline(never)]
fn test_insert2(a: EnumSet<SparseEnum>, b: SparseEnum) -> EnumSet<SparseEnum> {
    black_box(black_box(a) | black_box(b))
}

#[inline(never)]
fn test_insert3(a: EnumSet<LargeEnum>, b: LargeEnum) -> EnumSet<LargeEnum> {
    black_box(black_box(a) | black_box(b))
}

#[inline(never)]
fn test_insert4(a: EnumSet<ArrayEnum>, b: ArrayEnum) -> EnumSet<ArrayEnum> {
    black_box(black_box(a) | black_box(b))
}

#[inline(never)]
fn test_print<T: Debug>(t: T) {
    println!("{} {t:?}", std::any::type_name::<T>());
}

#[inline(never)]
fn test_dump1(a: EnumSet<SmallEnum>) {
    for v in black_box(a) {
        test_print(v);
    }
}

#[inline(never)]
fn test_dump2(a: EnumSet<SparseEnum>) {
    for v in black_box(a) {
        test_print(v);
    }
}

#[inline(never)]
fn test_dump3(a: EnumSet<LargeEnum>) {
    for v in black_box(a) {
        test_print(v);
    }
}

#[inline(never)]
fn test_dump4(a: EnumSet<ArrayEnum>) {
    for v in black_box(a) {
        test_print(v);
    }
}

fn main() {
    println!("{:?}", test_insert1(enum_set!(SmallEnum::A | SmallEnum::C), SmallEnum::E));
    println!("{:?}", test_insert1(enum_set!(SmallEnum::A | SmallEnum::D), SmallEnum::F));
    test_dump1(enum_set!(SmallEnum::A | SmallEnum::C | SmallEnum::E));
    test_dump1(enum_set!(SmallEnum::A | SmallEnum::D | SmallEnum::F));

    println!("{:?}", test_insert2(enum_set!(SparseEnum::A | SparseEnum::C), SparseEnum::E));
    println!("{:?}", test_insert2(enum_set!(SparseEnum::A | SparseEnum::D), SparseEnum::F));
    test_dump2(enum_set!(SparseEnum::A | SparseEnum::C | SparseEnum::E));
    test_dump2(enum_set!(SparseEnum::A | SparseEnum::D | SparseEnum::F));

    println!("{:?}", test_insert3(enum_set!(LargeEnum::A | LargeEnum::C), LargeEnum::E));
    println!("{:?}", test_insert3(enum_set!(LargeEnum::A | LargeEnum::D), LargeEnum::F));
    test_dump3(enum_set!(LargeEnum::A | LargeEnum::C | LargeEnum::E));
    test_dump3(enum_set!(LargeEnum::A | LargeEnum::D | LargeEnum::F));

    println!("{:?}", test_insert4(enum_set!(ArrayEnum::A | ArrayEnum::C), ArrayEnum::E));
    println!("{:?}", test_insert4(enum_set!(ArrayEnum::A | ArrayEnum::D), ArrayEnum::F));
    test_dump4(enum_set!(ArrayEnum::A | ArrayEnum::C | ArrayEnum::E));
    test_dump4(enum_set!(ArrayEnum::A | ArrayEnum::D | ArrayEnum::F));
}