#![no_std]
#![no_main]
#![deny(warnings)]

use core::panic::PanicInfo;
use enumset::*;

extern crate cortex_m;
extern crate cortex_m_rt;
extern crate cortex_m_semihosting;
extern crate defmt;
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
    defmt::timestamp!("Starting MyProgram...");
    defmt::error!("{}", e);
    if e.contains(SmallEnum::C) {
        core::panic!("oh no!");
    }
}

#[panic_handler]
fn panic_std(_info: &PanicInfo) -> ! {
    loop { cortex_m::asm::wfi() }
}

#[defmt::panic_handler]
fn panic_defmt() -> ! {
    loop { cortex_m::asm::wfi() }
}

#[cortex_m_rt::exception]
unsafe fn HardFault(_frame: &cortex_m_rt::ExceptionFrame) -> ! {
    loop { cortex_m::asm::wfi() }
}