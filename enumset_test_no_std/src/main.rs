#![no_std]
#![feature(start)]

use core::panic::PanicInfo;
use enumset::*;

#[derive(EnumSetType)]
pub enum SmallEnum {
    A,
    B,
    C,
}

#[start]
fn main(_: isize, _: *const *const u8) -> isize {
    let e = SmallEnum::A | SmallEnum::B;
    if e.contains(SmallEnum::C) {
        panic!("oh no!");
    }
    0
}

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop {}
}
