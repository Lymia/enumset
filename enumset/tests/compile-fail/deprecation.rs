#![deny(deprecated)]

use enumset::*;

#[derive(Debug, EnumSetType)]
enum Test {
    A, B, C
}

fn main() {
    let set: EnumSet<Test> = enum_set!();
    println!("{:?}", set);
}