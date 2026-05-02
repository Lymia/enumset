#![deny(warnings)]

use enumset::*;
use std::fmt::{Debug, Display, Formatter};

#[derive(EnumSetType, Debug)]
#[enumset(repr = "u64")]
#[rustfmt::skip]
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
    assert_eq!(set.len(), 3);
    assert!(!set.has_bit(11));
    assert!(set.insert_bit(11));
    assert_eq!(set.len(), 4);
    assert!(set.has_bit(11));
    assert!(!set.insert_bit(11));
    assert_eq!(set.len(), 4);
    assert!(set.has_bit(11));
    assert!(set.remove_bit(11));
    assert_eq!(set.len(), 3);
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
    assert_eq!(set.len(), 9);
    assert_eq!(set.valid_len(), 4);

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

#[test]
fn test_constructors_and_helpers() {
    let empty = MixedEnumSet::<Enum>::new();
    assert!(empty.is_empty());
    assert_eq!(empty.len(), 0);
    assert_eq!(MixedEnumSet::<Enum>::empty(), empty);

    let all = MixedEnumSet::<Enum>::all();
    assert_eq!(all.len(), 8);
    assert!(!all.has_unknown_bits());
    assert_eq!(all, Enum::A | Enum::B | Enum::C | Enum::D | Enum::E | Enum::F | Enum::G | Enum::H);

    assert_eq!(MixedEnumSet::<Enum>::variant_count(), 8);

    let only = MixedEnumSet::<Enum>::only(Enum::C);
    assert_eq!(only.len(), 1);
    assert!(only.contains(Enum::C));

    assert_eq!(MixedEnumSet::<Enum>::bit_index(Enum::A), 10);
    assert_eq!(MixedEnumSet::<Enum>::bit_index(Enum::C), 22);
    assert!(MixedEnumSet::<Enum>::is_bit_valid(10));
    assert!(!MixedEnumSet::<Enum>::is_bit_valid(11));
}

#[test]
fn test_clear() {
    let mut set = MixedEnumSet::from(Enum::A | Enum::C);
    set.insert_bit(1);
    assert_eq!(set.len(), 3);
    assert!(!set.is_empty());
    set.clear();
    assert!(set.is_empty());
    assert_eq!(set.len(), 0);
    assert!(!set.has_unknown_bits());
}

#[test]
fn test_insert_remove_value() {
    let mut set = MixedEnumSet::<Enum>::new();
    assert!(set.insert(Enum::A));
    assert_eq!(set.len(), 1);
    assert!(!set.insert(Enum::A));
    assert_eq!(set.len(), 1);
    assert!(set.contains(Enum::A));
    assert!(set.remove(Enum::A));
    assert_eq!(set.len(), 0);
    assert!(!set.remove(Enum::A));
    assert!(!set.contains(Enum::A));
}

#[test]
fn test_insert_all_remove_all() {
    let mut set = MixedEnumSet::from(Enum::A);
    set.insert_bit(1);
    set.insert_all(Enum::B | Enum::C);
    assert_eq!(set.len(), 4);
    assert!(set.contains(Enum::A));
    assert!(set.contains(Enum::B));
    assert!(set.contains(Enum::C));
    assert!(set.has_bit(1));

    set.remove_all(Enum::A | Enum::B);
    assert_eq!(set.len(), 2);
    assert!(!set.contains(Enum::A));
    assert!(!set.contains(Enum::B));
    assert!(set.contains(Enum::C));
    assert!(set.has_bit(1));

    // remove_all also accepts MixedEnumSet (containing unknown bits)
    let mut other = MixedEnumSet::<Enum>::new();
    other.insert_bit(1);
    set.remove_all(other);
    assert_eq!(set.len(), 1);
    assert!(!set.has_bit(1));
    assert!(set.contains(Enum::C));
}

#[test]
fn test_set_algebra() {
    let a = MixedEnumSet::from(Enum::A | Enum::B | Enum::C);
    let b = MixedEnumSet::from(Enum::B | Enum::C | Enum::D);

    assert_eq!(a.union(b), Enum::A | Enum::B | Enum::C | Enum::D);
    assert_eq!(a.intersection(b), Enum::B | Enum::C);
    assert_eq!(a.difference(b), MixedEnumSet::from(Enum::A));
    assert_eq!(a.symmetrical_difference(b), Enum::A | Enum::D);

    // operator forms
    assert_eq!(a | b, Enum::A | Enum::B | Enum::C | Enum::D);
    assert_eq!(a & b, Enum::B | Enum::C);
    assert_eq!(a - b, MixedEnumSet::from(Enum::A));
    assert_eq!(a ^ b, Enum::A | Enum::D);

    // assignment forms
    let mut x = a;
    x |= b;
    assert_eq!(x, a | b);
    let mut x = a;
    x &= b;
    assert_eq!(x, a & b);
    let mut x = a;
    x -= b;
    assert_eq!(x, a - b);
    let mut x = a;
    x ^= b;
    assert_eq!(x, a ^ b);
}

#[test]
fn test_subset_superset_disjoint() {
    let a = MixedEnumSet::from(Enum::A | Enum::B);
    let b = MixedEnumSet::from(Enum::A | Enum::B | Enum::C);
    let c = MixedEnumSet::from(Enum::D | Enum::E);

    assert!(a.is_subset(b));
    assert!(b.is_superset(a));
    assert!(!a.is_subset(c));
    assert!(a.is_disjoint(c));
    assert!(!a.is_disjoint(b));
}

#[test]
fn test_complement_clears_unknown_bits() {
    let mut set = MixedEnumSet::from(Enum::A);
    set.insert_bit(1);
    set.insert_bit(3);
    assert_eq!(set.len(), 3);
    assert!(set.has_unknown_bits());

    // complement is restricted to valid variants only
    let comp = set.complement();
    assert_eq!(comp.len(), 7);
    assert!(!comp.has_unknown_bits());
    assert_eq!(comp, Enum::B | Enum::C | Enum::D | Enum::E | Enum::F | Enum::G | Enum::H);

    // full_complement preserves bit-flip across all 64 bits (incl. unknown bits)
    let full = set.full_complement();
    assert_eq!(full.len(), 64 - 3);
    assert!(full.has_unknown_bits());
    assert_eq!(full.as_repr(), !set.as_repr());

    // not operator behaves like complement (clears unknown bits)
    let not_set = !set;
    assert_eq!(not_set, comp);
}

#[test]
#[should_panic]
fn test_as_enumset_panics_on_unknown_bits() {
    let mut set = MixedEnumSet::from(Enum::A);
    set.insert_bit(1);
    let _ = set.as_enumset();
}

#[test]
fn test_from_array_and_enumset() {
    let from_array: MixedEnumSet<Enum> = MixedEnumSet::from([Enum::A, Enum::B, Enum::C, Enum::A]);
    assert_eq!(from_array, Enum::A | Enum::B | Enum::C);

    let es: EnumSet<Enum> = Enum::A | Enum::C | Enum::E;
    let mes: MixedEnumSet<Enum> = MixedEnumSet::from(es);
    assert_eq!(mes, es);

    // Cross-type equality
    assert_eq!(es, mes);
    assert_eq!(mes, es);

    // With unknown bits, MixedEnumSet != EnumSet
    let mut mes2 = mes;
    mes2.insert_bit(1);
    assert_eq!(mes2.len(), 4);
    assert_ne!(mes2, es);
    assert_ne!(es, mes2);
}

#[test]
fn test_from_repr_truncated_clears_unknown() {
    let raw: u64 = (1 << 1) | (1 << 10) | (1 << 22);
    let set = MixedEnumSet::<Enum>::from_repr_truncated(raw);
    assert!(!set.has_unknown_bits());
    assert_eq!(set, Enum::A | Enum::C);
}

#[test]
fn test_sum() {
    let parts = [
        EnumSet::from(Enum::A),
        EnumSet::from(Enum::B),
        EnumSet::from(Enum::C),
    ];
    let sum: MixedEnumSet<Enum> = parts.iter().copied().sum();
    assert_eq!(sum, Enum::A | Enum::B | Enum::C);

    let sum_ref: MixedEnumSet<Enum> = parts.iter().sum();
    assert_eq!(sum_ref, Enum::A | Enum::B | Enum::C);
}

#[test]
fn test_mixed_value_fmt() {
    let valid: MixedValue<Enum> = MixedValue::Valid(Enum::A);
    let invalid: MixedValue<Enum> = MixedValue::Invalid(7);
    assert_eq!(format!("{valid:?}"), "A");
    assert_eq!(format!("{invalid:?}"), "[7]");
    assert_eq!(format!("{valid}"), "A");
    assert_eq!(format!("{invalid}"), "[7]");
}

#[test]
fn test_mixed_enum_set_macro() {
    const CONST_SET: MixedEnumSet<Enum> = mixed_enum_set!(Enum::A | Enum::B | Enum::C);
    assert_eq!(CONST_SET, MixedEnumSet::from(Enum::A | Enum::B | Enum::C));

    const EMPTY: MixedEnumSet<Enum> = mixed_enum_set!();
    assert!(EMPTY.is_empty());
    assert_eq!(EMPTY.len(), 0);
}

#[test]
fn test_iter_len_and_size_hint() {
    let mut set = MixedEnumSet::from(Enum::A | Enum::C);
    set.insert_bit(1);
    let iter = set.iter();
    assert_eq!(iter.len(), 3);
    assert_eq!(iter.size_hint(), (3, Some(3)));
}
