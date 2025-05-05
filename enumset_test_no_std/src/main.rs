#![no_std]
#![deny(warnings)]

use enumset::*;

#[derive(EnumSetType)]
pub enum SmallEnum {
    A,
    B,
    C,
}

fn main() {
    let e = SmallEnum::A | SmallEnum::B;
    if e.contains(SmallEnum::C) {
        panic!("oh no!");
    }
}