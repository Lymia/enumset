#![no_std]
#![no_main]
#![deny(warnings)]

use core::panic::PanicInfo;
use enumset::*;

#[derive(EnumSetType)]
pub enum SmallEnum {
    A,
    B,
    C,
}

#[no_mangle]
fn _start() {
    let e = SmallEnum::A | SmallEnum::B;
    if e.contains(SmallEnum::C) {
        panic!("oh no!");
    }
}

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop {}
}
