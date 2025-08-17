#![deny(warnings)]

use std::fmt::{Debug, Display, Formatter};
use enumset::*;

#[derive(EnumSetType, Debug)]
#[enumset(repr = "u64")]
enum Enum {
    A = 0xA, B = 15, C = 22, D = 42, E = 55, F, G, H,
}
impl Display for Enum {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}

#[test]
fn test_insert_remove() {
    let mut set = MixedEnumSet::from(Enum::A | Enum::C | Enum::E);
    assert!(!set.contains(Enum::B));
    assert!(!set.contains(Enum::D));
    assert!(!set.contains(Enum::F));
    assert!(!set.contains(Enum::G));
    assert!(!set.contains(Enum::H));
    assert!(set.contains(Enum::A));
    assert!(set.contains(Enum::C));
    assert!(set.contains(Enum::E));
    assert!(set.has_bit(10));

    // repeated insert and remove
    assert!(!set.has_bit(11));
    assert!(set.insert_bit(11));
    assert!(set.has_bit(11));
    assert!(!set.insert_bit(11));
    assert!(set.has_bit(11));
    assert!(set.remove_bit(11));
    assert!(!set.has_bit(11));

    // bit and enum equivalence
    assert!(set.remove_bit(10));
    assert!(!set.contains(Enum::A));
    assert!(!set.has_bit(10));
    assert!(set.insert_bit(10));
    assert!(set.contains(Enum::A));
    assert!(set.has_bit(10));
}

#[test]
fn test_repr() {
    let mut set = MixedEnumSet::from(Enum::A | Enum::C | Enum::E);
    set.insert_bit(1);
    assert_eq!(set.as_repr(), (1 << 1) | (1 << 10) | (1 << 22) | (1 << 55));
    assert_eq!(set, MixedEnumSet::<Enum>::from_repr(set.as_repr()));
    let set2 = MixedEnumSet::<Enum>::from_repr_truncated(set.full_complement().as_repr());
    assert_eq!(set2, Enum::B | Enum::D | Enum::F | Enum::G | Enum::H);
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
    assert_eq!(set.iter().count(), 64);
}

#[test]
fn test_iter_valid_len() {
    let mut set = MixedEnumSet::from(Enum::A | Enum::C | Enum::E | Enum::G);
    assert_eq!(set.valid_len(), 4);
    set.insert_bit(1);
    assert_eq!(set.valid_len(), 4);
    set.insert_bit(3);
    assert_eq!(set.valid_len(), 4);
    set.insert_bit(5);
    assert_eq!(set.valid_len(), 4);
    set.insert_bit(50);
    assert_eq!(set.valid_len(), 4);
    set.insert_bit(60);
    assert_eq!(set.valid_len(), 4);

    assert_eq!(set.len(), 9);
    assert_eq!(set.complement().len(), 4);
    assert_eq!(set.full_complement().len(), 64 - 9);
}

#[test]
fn test_fmt() {
    let mut set = MixedEnumSet::from(Enum::A | Enum::C | Enum::E);
    set.insert_bit(1);
    assert_eq!(format!("{set:?}"), "MixedEnumSet([1] | A | C | E)");
    assert_eq!(set.to_string(), "[1] | A | C | E");
}

#[test]
fn test_extend() {
    let mut set = MixedEnumSet::from(Enum::A);
    set.extend(Enum::B | Enum::C);
    assert_eq!(set, Enum::A | Enum::B | Enum::C);
    set.extend(&[Enum::E | Enum::F]);
    assert_eq!(set, Enum::A | Enum::B | Enum::C | Enum::E | Enum::F);
    set.extend([Enum::G | Enum::H]);
    assert_eq!(set, Enum::A | Enum::B | Enum::C | Enum::E | Enum::F | Enum::G | Enum::H);
}

#[test]
fn test_from_iterator() {
    let set = MixedEnumSet::from_iter([Enum::A, Enum::B, Enum::C]);
    assert_eq!(set, Enum::A | Enum::B | Enum::C);
    let set = MixedEnumSet::from_iter(&[Enum::A, Enum::B, Enum::C]);
    assert_eq!(set, Enum::A | Enum::B | Enum::C);
    let set = MixedEnumSet::from_iter([Enum::A | Enum::B, Enum::B | Enum::C]);
    assert_eq!(set, Enum::A | Enum::B | Enum::C);
    let set = MixedEnumSet::from_iter(&[Enum::A | Enum::B, Enum::B | Enum::C]);
    assert_eq!(set, Enum::A | Enum::B | Enum::C);
}