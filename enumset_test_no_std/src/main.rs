#![no_std]
#![no_main]
#![deny(warnings)]

use core::panic::PanicInfo;
use enumset::*;

extern crate defmt_semihosting;

#[derive(EnumSetType, defmt::Format)]
pub enum SmallEnum {
    A,
    B,
    C,
}

#[no_mangle]
pub fn _start() {
    let e = SmallEnum::A | SmallEnum::B;
    defmt::error!("{}", e);
    if e.contains(SmallEnum::C) {
        core::panic!("oh no!");
    }
}

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop {}
}
