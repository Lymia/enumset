extern crate enumset as enumst2;

#[derive(enumst2::EnumSetType)]
#[enumset(crate_name = "enumst2")]
enum Test {
    A, B, C, D,
}

fn main() {}