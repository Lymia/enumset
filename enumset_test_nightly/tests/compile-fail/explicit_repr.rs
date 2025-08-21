use enumset::*;
use enumset::set::*;

#[derive(EnumSetType)]
enum OkayEnumButCantUseFromRepr {
    Variant,
}

#[derive(EnumSetType)]
#[enumset(repr = "array")]
enum OkayEnumButCantUseFromReprArray {
    Variant,
}

fn main() {
    EnumSet::<OkayEnumButCantUseFromRepr>::from_repr(1);
    EnumSet::<OkayEnumButCantUseFromReprArray>::from_repr([1]);
    MixedEnumSet::<OkayEnumButCantUseFromRepr>::from_repr(1);
}
