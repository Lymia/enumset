use criterion::{criterion_group, criterion_main, Criterion};
use enumset::{enum_set, EnumSet, EnumSetType};
use std::hint::black_box;

// Test of a relatively typical enum.
#[derive(EnumSetType, Debug)]
#[rustfmt::skip]
enum SmallEnum {
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z,
}

/// Used to test enums with sparse elements.
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

macro_rules! test_enum {
    ($c:expr, $name:expr, $ty:tt) => {{
        $c.bench_function(concat!($name, "_iter"), |b| {
            b.iter(|| {
                let e = black_box(enum_set!($ty::A | $ty::C | $ty::D | $ty::H));
                for i in e {
                    black_box(i);
                }
            })
        });
        $c.bench_function(concat!($name, "_insert"), |b| {
            b.iter(|| {
                let mut e = EnumSet::<$ty>::new();
                for i in [$ty::A, $ty::C, $ty::D, $ty::H] {
                    e.insert(black_box(i));
                }
                black_box(e);
            })
        });
    }};
}

fn criterion_benchmark(c: &mut Criterion) {
    test_enum!(c, "small_enum", SmallEnum);
    test_enum!(c, "sparse_enum", SparseEnum);
    test_enum!(c, "large_enum", LargeEnum);
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
