#![no_std]
#![no_main]
#![deny(warnings)]

use enumset::*;

extern crate cortex_m;
extern crate cortex_m_rt;
extern crate defmt;
extern crate defmt_semihosting;

#[derive(EnumSetType, defmt::Format)]
#[rustfmt::skip]
pub enum SmallEnum {
    A, B, C,
}

#[derive(EnumSetType, defmt::Format)]
#[rustfmt::skip]
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

#[cortex_m_rt::entry]
fn main() -> ! {
    defmt::error!("Hello, world!");
    defmt::error!("Set: SmallEnum = {}", SmallEnum::A | SmallEnum::B);
    defmt::error!("Set: ArrayEnum = {}", ArrayEnum::A | ArrayEnum::C | ArrayEnum::E | ArrayEnum::X);
    semihosting::process::exit(0)
}

/// A panic handler which logs to defmt and then does a semihosting exit.
#[panic_handler]
fn panic(info: &core::panic::PanicInfo) -> ! {
    if let Some(location) = info.location() {
        defmt::error!(
            "Panic! {} ({=str}:{=u32})",
            defmt::Debug2Format(&info.message()),
            location.file(),
            location.line()
        );
    } else {
        defmt::error!("Panic! {}", defmt::Debug2Format(&info.message()));
    }
    semihosting::process::exit(1)
}

/// A Hard Fault handler which logs to defmt and then does a semihosting exit.
#[cortex_m_rt::exception(trampoline = true)]
unsafe fn HardFault(frame: &cortex_m_rt::ExceptionFrame) -> ! {
    defmt::error!("HardFault: {}", defmt::Debug2Format(frame));
    semihosting::process::exit(1)
}
