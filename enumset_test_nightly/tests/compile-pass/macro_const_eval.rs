use enumset::*;

#[derive(EnumSetType, Debug)]
#[rustfmt::skip]
enum TestEnum {
    First = (1 + 10) / 3,         // ==   3
    Second = (4 % 3),             // ==   1
    Variant2 = (4 * 4) - 4,       // ==  12
    Variant3 = !1 & 7,            // ==   6
    Variant4 = 1 ^ 3,             // ==   2
    Variant5 = 3 ^ 14,            // ==  13
    Variant6 = 256 >> 1,          // == 128
    Variant7 = 1 + 1 + 1 + 1 + 1, // ==   5
    Variant8 = 1 * 2 * 3 + 4 * 5, // ==  26
}

macro_rules! test_2 {
    ($grouped:expr) => {
        #[derive(EnumSetType, Debug)]
        enum Enum2 {
            First = $grouped * 2,
            Second,
        }
    };
}
test_2!(1 + 1);

pub fn main() {
    assert_eq!(Enum2::First as u32, 4);
}