use criterion::{black_box, criterion_group, criterion_main, Criterion};
use enumset::{EnumSet, EnumSetType};

#[derive(EnumSetType)]
enum SmallExample {
    A,
    B,
    C,
    D,
}

#[derive(EnumSetType)]
#[rustfmt::skip]
enum LargeExample {
    A1, A2, A3, A4, A5, A6, A7, A8,
    B1, B2, B3, B4, B5, B6, B7, B8,
    C1, C2, C3, C4, C5, C6, C7, C8,
    D1, D2, D3, D4, D5, D6, D7, D8,
    E1, E2, E3, E4, E5, E6, E7, E8,
    F1, F2, F3, F4, F5, F6, F7, F8,
}

#[derive(EnumSetType)]
#[rustfmt::skip]
enum HugeExample {
    A1, A2, A3, A4, A5, A6, A7, A8, A9, A0,
    B1, B2, B3, B4, B5, B6, B7, B8, B9, B0,
    C1, C2, C3, C4, C5, C6, C7, C8, C9, C0,
    D1, D2, D3, D4, D5, D6, D7, D8, D9, D0,
    E1, E2, E3, E4, E5, E6, E7, E8, E9, E0,
    F1, F2, F3, F4, F5, F6, F7, F8, F9, F0,
    G1, G2, G3, G4, G5, G6, G7, G8, G9, G0,
    H1, H2, H3, H4, H5, H6, H7, H8, H9, H0,
    I1, I2, I3, I4, I5, I6, I7, I8, I9, I0,
    J1, J2, J3, J4, J5, J6, J7, J8, J9, J0,
}

fn benchmark(c: &mut Criterion) {
    // Enum sets containing all variants
    c.benchmark_group("iterate-full")
        .bench_function("small", |b| {
            let set = EnumSet::<SmallExample>::all();
            b.iter(|| iterate(black_box(set)))
        })
        .bench_function("large", |b| {
            let set = EnumSet::<LargeExample>::all();
            b.iter(|| iterate(black_box(set)))
        })
        .bench_function("huge", |b| {
            let set = EnumSet::<HugeExample>::all();
            b.iter(|| iterate(black_box(set)))
        });

    // Enum sets containing half of the variants
    c.benchmark_group("iterate-half")
        .bench_function("small", |b| {
            let mut set = EnumSet::<SmallExample>::empty();
            for i in (0..EnumSet::<SmallExample>::all().len()).step_by(2) {
                set |= EnumSet::from_u128(1 << i);
            }
            b.iter(|| iterate(black_box(set)))
        })
        .bench_function("large", |b| {
            let mut set = EnumSet::<LargeExample>::empty();
            for i in (0..EnumSet::<LargeExample>::all().len()).step_by(2) {
                set |= EnumSet::from_u128(1 << i);
            }
            b.iter(|| iterate(black_box(set)))
        })
        .bench_function("huge", |b| {
            let mut set = EnumSet::<HugeExample>::empty();
            for i in (0..EnumSet::<HugeExample>::all().len()).step_by(2) {
                set |= EnumSet::from_u128(1 << i);
            }
            b.iter(|| iterate(black_box(set)))
        });

    // Enum sets containing roughly log2 variants
    c.benchmark_group("iterate-sparse")
        .bench_function("small", |b| {
            let set = SmallExample::A | SmallExample::D;
            b.iter(|| iterate(black_box(set)))
        })
        .bench_function("large", |b| {
            let set = LargeExample::A1
                | LargeExample::C2
                | LargeExample::D7
                | LargeExample::B3
                | LargeExample::E4
                | LargeExample::F8;
            b.iter(|| iterate(black_box(set)))
        })
        .bench_function("huge", |b| {
            let set = HugeExample::A1
                | HugeExample::C3
                | HugeExample::D9
                | HugeExample::H6
                | HugeExample::E8
                | HugeExample::I4
                | HugeExample::J0;
            b.iter(|| iterate(black_box(set)))
        });

    // Enum sets containing a single variant
    c.benchmark_group("iterate-single")
        .bench_function("small", |b| {
            let set = EnumSet::only(SmallExample::C);
            b.iter(|| iterate(black_box(set)))
        })
        .bench_function("large", |b| {
            let set = EnumSet::only(LargeExample::C8);
            b.iter(|| iterate(black_box(set)))
        })
        .bench_function("huge", |b| {
            let set = EnumSet::only(HugeExample::E0);
            b.iter(|| iterate(black_box(set)))
        });

    // Enum sets containing no variants
    c.benchmark_group("iterate-empty")
        .bench_function("small", |b| {
            let set = EnumSet::<SmallExample>::empty();
            b.iter(|| iterate(black_box(set)))
        })
        .bench_function("large", |b| {
            let set = EnumSet::<LargeExample>::empty();
            b.iter(|| iterate(black_box(set)))
        })
        .bench_function("huge", |b| {
            let set = EnumSet::<HugeExample>::empty();
            b.iter(|| iterate(black_box(set)))
        });
}

fn iterate<T: EnumSetType>(set: EnumSet<T>) {
    for v in set {
        black_box(v);
    }
}

criterion_group! {
    name = benches;
    config = Criterion::default()
        .measurement_time(std::time::Duration::from_secs(30))
        .sample_size(5000);
    targets = benchmark
}
criterion_main!(benches);
