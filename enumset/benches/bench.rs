use criterion::{black_box, criterion_group, criterion_main, Criterion};
use enumset::*;
use enumset::__internal::*;
use std::mem::transmute;

#[derive(EnumSetType)]
#[repr(u8)]
enum Enum8 {
    A, B, C, D, E, F, G, H,
}

#[derive(EnumSetType)]
#[repr(u64)]
enum Enum64 {
    A, B, C, D, E, F, G, H,
}

#[derive(EnumSetType)]
#[repr(C)]
enum EnumC {
    A, B, C, D, E, F, G, H,
}

fn transmute_enum(c: &mut Criterion) {
    c.bench_function("transmute u8", |b| b.iter(|| unsafe {
        transmute::<u8, Enum8>(black_box(0u32) as u8)
    }));
    c.bench_function("transmute u64", |b| b.iter(|| unsafe {
        transmute::<u64, Enum64>(black_box(0u32) as u64)
    }));
    c.bench_function("transmute C", |b| b.iter(|| unsafe {
        transmute::<u32, EnumC>(black_box(0u32) as u32)
    }));
}
fn from_enum(c: &mut Criterion) {
    c.bench_function("enum_from_u32 u8", |b| b.iter(|| unsafe {
        <Enum8 as EnumSetTypePrivate>::enum_from_u32(black_box(0u32))
    }));
    c.bench_function("enum_from_u32 u64", |b| b.iter(|| unsafe {
        <Enum64 as EnumSetTypePrivate>::enum_from_u32(black_box(0u32))
    }));
    c.bench_function("enum_from_u32 C", |b| b.iter(|| unsafe {
        <EnumC as EnumSetTypePrivate>::enum_from_u32(black_box(0u32))
    }));
}

criterion_group!(baseline, transmute_enum);
criterion_group!(ops, from_enum);
criterion_main!(baseline, ops);
