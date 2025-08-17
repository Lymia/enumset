#![deny(warnings)]

use enumset::*;

#[derive(EnumSetType, Debug)]
#[enumset(repr = "u64")]
enum Enum {
    A = 0xA, B = 15, C = 22, D = 42, E = 55, F, G, H,
}

#[test]
fn test_sparse_conversions() {
    let conversions = MixedEnumSet::from(Enum::A | Enum::C | Enum::E);
    assert!(!conversions.has_unknown_bits());
    assert_eq!(conversions.as_enumset(), conversions);
    assert_eq!(conversions.as_enumset_truncate(), conversions);

    let mut new = conversions;
    new.insert_bit(1);
    assert!(new.has_unknown_bits());
    assert_eq!(new.try_as_enumset(), None);
    assert_eq!(new.as_enumset_truncate(), conversions);

    assert_eq!(format!("{new:?}"), "MixedEnumSet([1] | A | C | E)");
}

#[test]
fn test_iter_mixed() {
    let mut set = MixedEnumSet::from(Enum::A | Enum::C | Enum::E | Enum::G);
    set.insert_bit(1);
    set.insert_bit(3);
    set.insert_bit(5);
    set.insert_bit(50);
    set.insert_bit(60);

    let mut iter = set.iter();
    assert_eq!(iter.next(), Some(MixedValue::Invalid(1)));
    assert_eq!(iter.next(), Some(MixedValue::Invalid(3)));
    assert_eq!(iter.next_back(), Some(MixedValue::Invalid(60)));
    assert_eq!(iter.next_back(), Some(MixedValue::Valid(Enum::G)));
    assert_eq!(iter.next(), Some(MixedValue::Invalid(5)));
    assert_eq!(iter.next(), Some(MixedValue::Valid(Enum::A)));
    assert_eq!(iter.next(), Some(MixedValue::Valid(Enum::C)));
    assert_eq!(iter.next_back(), Some(MixedValue::Valid(Enum::E)));
    assert_eq!(iter.next(), Some(MixedValue::Invalid(50)));
    assert_eq!(iter.next(), None);
}

#[test]
fn test_iter_size() {
    let set = MixedEnumSet::<Enum>::from_repr(!0);
    let mut count = 0;
    for _ in set {
        count += 1;
    }
    assert_eq!(count, 64);
}