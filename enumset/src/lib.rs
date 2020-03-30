#![no_std]
#![forbid(missing_docs)]

//! A library for defining enums that can be used in compact bit sets. It supports enums up to 128
//! variants, and has a macro to use these sets in constants.
//!
//! For serde support, enable the `serde` feature.
//!
//! # Defining enums for use with EnumSet
//!
//! Enums to be used with [`EnumSet`] should be defined using `#[derive(EnumSetType)]`:
//!
//! ```rust
//! # use enumset::*;
//! #[derive(EnumSetType, Debug)]
//! pub enum Enum {
//!    A, B, C, D, E, F, G,
//! }
//! ```
//!
//! # Working with EnumSets
//!
//! EnumSets can be constructed via [`EnumSet::new()`] like a normal set. In addition,
//! `#[derive(EnumSetType)]` creates operator overloads that allow you to create EnumSets like so:
//!
//! ```rust
//! # use enumset::*;
//! # #[derive(EnumSetType, Debug)] pub enum Enum { A, B, C, D, E, F, G }
//! let new_set = Enum::A | Enum::C | Enum::G;
//! assert_eq!(new_set.len(), 3);
//! ```
//!
//! All bitwise operations you would expect to work on bitsets also work on both EnumSets and
//! enums with `#[derive(EnumSetType)]`:
//! ```rust
//! # use enumset::*;
//! # #[derive(EnumSetType, Debug)] pub enum Enum { A, B, C, D, E, F, G }
//! // Intersection of sets
//! assert_eq!((Enum::A | Enum::B) & Enum::C, EnumSet::empty());
//! assert_eq!((Enum::A | Enum::B) & Enum::A, Enum::A);
//! assert_eq!(Enum::A & Enum::B, EnumSet::empty());
//!
//! // Symmetric difference of sets
//! assert_eq!((Enum::A | Enum::B) ^ (Enum::B | Enum::C), Enum::A | Enum::C);
//! assert_eq!(Enum::A ^ Enum::C, Enum::A | Enum::C);
//!
//! // Difference of sets
//! assert_eq!((Enum::A | Enum::B | Enum::C) - Enum::B, Enum::A | Enum::C);
//!
//! // Complement of sets
//! assert_eq!(!(Enum::E | Enum::G), Enum::A | Enum::B | Enum::C | Enum::D | Enum::F);
//! ```
//!
//! The [`enum_set!`] macro allows you to create EnumSets in constant contexts:
//!
//! ```rust
//! # use enumset::*;
//! # #[derive(EnumSetType, Debug)] pub enum Enum { A, B, C, D, E, F, G }
//! const CONST_SET: EnumSet<Enum> = enum_set!(Enum::A | Enum::B);
//! assert_eq!(CONST_SET, Enum::A | Enum::B);
//! ```
//!
//! Mutable operations on the [`EnumSet`] otherwise work basically as expected:
//!
//! ```rust
//! # use enumset::*;
//! # #[derive(EnumSetType, Debug)] pub enum Enum { A, B, C, D, E, F, G }
//! let mut set = EnumSet::new();
//! set.insert(Enum::A);
//! set.insert_all(Enum::E | Enum::G);
//! assert!(set.contains(Enum::A));
//! assert!(!set.contains(Enum::B));
//! assert_eq!(set, Enum::A | Enum::E | Enum::G);
//! ```

pub use enumset_derive::*;

use core::cmp::Ordering;
use core::fmt;
use core::fmt::{Debug, Formatter};
use core::hash::{Hash, Hasher};
use core::iter::FromIterator;
use core::ops::*;

use num_traits::*;

#[doc(hidden)]
/// Everything in this module is internal API and may change at any time.
pub mod internal {
    use super::*;

    /// A struct used to type check [`enum_set!`].
    pub struct EnumSetSameTypeHack<'a, T: EnumSetType + 'static> {
        pub unified: &'a [T],
        pub enum_set: EnumSet<T>,
    }

    /// A reexport of core to allow our macros to be generic to std vs core.
    pub use ::core as core_export;

    /// A reexport of serde so there is no requirement to depend on serde.
    #[cfg(feature = "serde")] pub use serde2 as serde;

    /// The actual members of EnumSetType. Put here to avoid polluting global namespaces.
    pub unsafe trait EnumSetTypePrivate {
        type Repr: EnumSetTypeRepr;
        const ALL_BITS: Self::Repr;
        fn enum_into_u8(self) -> u8;
        unsafe fn enum_from_u8(val: u8) -> Self;

        #[cfg(feature = "serde")]
        fn serialize<S: serde::Serializer>(set: EnumSet<Self>, ser: S) -> Result<S::Ok, S::Error>
            where Self: EnumSetType;
        #[cfg(feature = "serde")]
        fn deserialize<'de, D: serde::Deserializer<'de>>(de: D) -> Result<EnumSet<Self>, D::Error>
            where Self: EnumSetType;
    }
}
use crate::internal::EnumSetTypePrivate;
#[cfg(feature = "serde")] use crate::internal::serde;
#[cfg(feature = "serde")] use crate::serde::{Serialize, Deserialize};

mod private {
    use super::*;
    pub trait EnumSetTypeRepr : PrimInt + FromPrimitive + WrappingSub + CheckedShl + Debug + Hash {
        const WIDTH: u8;
    }
    macro_rules! prim {
        ($name:ty, $width:expr) => {
            impl EnumSetTypeRepr for $name {
                const WIDTH: u8 = $width;
            }
        }
    }
    prim!(u8  , 8  );
    prim!(u16 , 16 );
    prim!(u32 , 32 );
    prim!(u64 , 64 );
    prim!(u128, 128);
}
use crate::private::EnumSetTypeRepr;

/// The trait used to define enum types that may be used with [`EnumSet`].
///
/// This trait should be implemented using `#[derive(EnumSetType)]`. Its internal structure is
/// not currently stable, and may change at any time.
///
/// # Custom Derive
///
/// The custom derive for [`EnumSetType`] automatically creates implementations of [`PartialEq`],
/// [`Sub`], [`BitAnd`], [`BitOr`], [`BitXor`], and [`Not`] allowing the enum to be used as
/// if it were an [`EnumSet`] in expressions. This can be disabled by adding an `#[enumset(no_ops)]`
/// annotation to the enum.
///
/// The custom derive for `EnumSetType` automatically implements [`Copy`], [`Clone`], [`Eq`], and
/// [`PartialEq`] on the enum. These are required for the [`EnumSet`] to function.
///
/// Any C-like enum is supported, as long as there are no more than 128 variants in the enum,
/// and no variant discriminator is larger than 127.
///
/// # Examples
///
/// Deriving a plain EnumSetType:
///
/// ```rust
/// # use enumset::*;
/// #[derive(EnumSetType)]
/// pub enum Enum {
///    A, B, C, D, E, F, G,
/// }
/// ```
///
/// Deriving a sparse EnumSetType:
///
/// ```rust
/// # use enumset::*;
/// #[derive(EnumSetType)]
/// pub enum SparseEnum {
///    A = 10, B = 20, C = 30, D = 127,
/// }
/// ```
///
/// Deriving an EnumSetType without adding ops:
///
/// ```rust
/// # use enumset::*;
/// #[derive(EnumSetType)]
/// #[enumset(no_ops)]
/// pub enum NoOpsEnum {
///    A, B, C, D, E, F, G,
/// }
/// ```
pub unsafe trait EnumSetType: Copy + Eq + EnumSetTypePrivate { }

/// An efficient set type for enums.
///
/// It is implemented using a bitset stored using the smallest integer that can fit all bits
/// in the underlying enum.
///
/// # Serialization
///
/// By default, `EnumSet`s are serialized as an unsigned integer of the same width as used to store
/// it in memory.
///
/// Unknown bits are ignored, and are simply dropped. To override this behavior, you can add a
/// `#[enumset(serialize_deny_unknown)]` annotation to your enum.
///
/// You can add a `#[enumset(serialize_repr = "u8")]` annotation to your enum to manually set
/// the number width the `EnumSet` is serialized as. Only unsigned integer types may be used. This
/// can be used to avoid breaking changes in certain serialization formats (such as `bincode`).
///
/// In addition, the `#[enumset(serialize_as_list)]` annotation causes the `EnumSet` to be
/// instead serialized as a list of enum variants. This requires your enum type implement
/// [`Serialize`] and [`Deserialize`].
#[derive(Copy, Clone, PartialEq, Eq)]
pub struct EnumSet<T : EnumSetType> {
    #[doc(hidden)]
    /// This is public due to the [`enum_set!`] macro.
    /// This is **NOT** public API and may change at any time.
    pub __enumset_underlying: T::Repr
}
impl <T : EnumSetType> EnumSet<T> {
    fn mask(bit: u8) -> T::Repr {
        Shl::<usize>::shl(T::Repr::one(), bit as usize)
    }
    fn has_bit(&self, bit: u8) -> bool {
        let mask = Self::mask(bit);
        self.__enumset_underlying & mask == mask
    }
    fn partial_bits(bits: u8) -> T::Repr {
        T::Repr::one().checked_shl(bits.into())
            .unwrap_or(T::Repr::zero())
            .wrapping_sub(&T::Repr::one())
    }

    // Returns all bits valid for the enum
    fn all_bits() -> T::Repr {
        T::ALL_BITS
    }

    /// Creates an empty `EnumSet`.
    pub fn new() -> Self {
        EnumSet { __enumset_underlying: T::Repr::zero() }
    }

    /// Returns an `EnumSet` containing a single element.
    pub fn only(t: T) -> Self {
        EnumSet { __enumset_underlying: Self::mask(t.enum_into_u8()) }
    }

    /// Creates an empty `EnumSet`.
    ///
    /// This is an alias for [`EnumSet::new`].
    pub fn empty() -> Self {
        Self::new()
    }

    /// Returns an `EnumSet` containing all valid variants of the enum.
    pub fn all() -> Self {
        EnumSet { __enumset_underlying: Self::all_bits() }
    }

    /// Total number of bits used by this type. Note that the actual amount of space used is
    /// rounded up to the next highest integer type (`u8`, `u16`, `u32`, `u64`, or `u128`).
    ///
    /// This is the same as [`EnumSet::variant_count`] except in enums with "sparse" variants.
    /// (e.g. `enum Foo { A = 10, B = 20 }`)
    pub fn bit_width() -> u8 {
        T::Repr::WIDTH - T::ALL_BITS.leading_zeros() as u8
    }

    /// The number of valid variants that this type can contain.
    ///
    /// This is the same as [`EnumSet::bit_width`] except in enums with "sparse" variants.
    /// (e.g. `enum Foo { A = 10, B = 20 }`)
    pub fn variant_count() -> u8 {
        T::ALL_BITS.count_ones() as u8
    }

    /// Returns the raw bits of this set.
    pub fn to_bits(&self) -> u128 {
        self.__enumset_underlying.to_u128()
            .expect("Impossible: Bits cannot be to converted into i128?")
    }

    /// Constructs a bitset from raw bits.
    ///
    /// # Panics
    /// If bits not in the enum are set.
    pub fn from_bits(bits: u128) -> Self {
        assert!((bits & !Self::all().to_bits()) == 0, "Bits not valid for the enum were set.");
        EnumSet {
            __enumset_underlying: T::Repr::from_u128(bits)
                .expect("Impossible: Valid bits too large to fit in repr?")
        }
    }

    /// Constructs a bitset from raw bits, ignoring any unknown variants.
    pub fn from_bits_safe(bits: u128) -> Self {
        Self::form_bits(bits & Self::all().to_bits())
    }

    /// Returns the number of elements in this set.
    pub fn len(&self) -> usize {
        self.__enumset_underlying.count_ones() as usize
    }
    /// Returns `true` if the set contains no elements.
    pub fn is_empty(&self) -> bool {
        self.__enumset_underlying.is_zero()
    }
    /// Removes all elements from the set.
    pub fn clear(&mut self) {
        self.__enumset_underlying = T::Repr::zero()
    }

    /// Returns `true` if `self` has no elements in common with `other`. This is equivalent to
    /// checking for an empty intersection.
    pub fn is_disjoint(&self, other: Self) -> bool {
        (*self & other).is_empty()
    }
    /// Returns `true` if the set is a superset of another, i.e., `self` contains at least all the
    /// values in `other`.
    pub fn is_superset(&self, other: Self) -> bool {
        (*self & other).__enumset_underlying == other.__enumset_underlying
    }
    /// Returns `true` if the set is a subset of another, i.e., `other` contains at least all
    /// the values in `self`.
    pub fn is_subset(&self, other: Self) -> bool {
        other.is_superset(*self)
    }

    /// Returns a set containing any elements present in either set.
    pub fn union(&self, other: Self) -> Self {
        EnumSet { __enumset_underlying: self.__enumset_underlying | other.__enumset_underlying }
    }
    /// Returns a set containing every element present in both sets.
    pub fn intersection(&self, other: Self) -> Self {
        EnumSet { __enumset_underlying: self.__enumset_underlying & other.__enumset_underlying }
    }
    /// Returns a set containing element present in `self` but not in `other`.
    pub fn difference(&self, other: Self) -> Self {
        EnumSet { __enumset_underlying: self.__enumset_underlying & !other.__enumset_underlying }
    }
    /// Returns a set containing every element present in either `self` or `other`, but is not
    /// present in both.
    pub fn symmetrical_difference(&self, other: Self) -> Self {
        EnumSet { __enumset_underlying: self.__enumset_underlying ^ other.__enumset_underlying }
    }
    /// Returns a set containing all enum variants not in this set.
    pub fn complement(&self) -> Self {
        EnumSet { __enumset_underlying: !self.__enumset_underlying & Self::all_bits() }
    }

    /// Checks whether this set contains a value.
    pub fn contains(&self, value: T) -> bool {
        self.has_bit(value.enum_into_u8())
    }

    /// Adds a value to this set.
    ///
    /// If the set did not have this value present, `false` is returned.
    ///
    /// If the set did have this value present, `true` is returned.
    pub fn insert(&mut self, value: T) -> bool {
        let contains = !self.contains(value);
        self.__enumset_underlying = self.__enumset_underlying | Self::mask(value.enum_into_u8());
        contains
    }
    /// Removes a value from this set. Returns whether the value was present in the set.
    pub fn remove(&mut self, value: T) -> bool {
        let contains = self.contains(value);
        self.__enumset_underlying = self.__enumset_underlying & !Self::mask(value.enum_into_u8());
        contains
    }

    /// Adds all elements in another set to this one.
    pub fn insert_all(&mut self, other: Self) {
        self.__enumset_underlying = self.__enumset_underlying | other.__enumset_underlying
    }
    /// Removes all values in another set from this one.
    pub fn remove_all(&mut self, other: Self) {
        self.__enumset_underlying = self.__enumset_underlying & !other.__enumset_underlying
    }

    /// Creates an iterator over the values in this set.
    pub fn iter(&self) -> EnumSetIter<T> {
        EnumSetIter(*self, 0)
    }
}

impl <T: EnumSetType> Default for EnumSet<T> {
    /// Returns an empty set.
    fn default() -> Self {
        Self::new()
    }
}

impl <T : EnumSetType> IntoIterator for EnumSet<T> {
    type Item = T;
    type IntoIter = EnumSetIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl <T : EnumSetType, O: Into<EnumSet<T>>> Sub<O> for EnumSet<T> {
    type Output = Self;
    fn sub(self, other: O) -> Self::Output {
        self.difference(other.into())
    }
}
impl <T : EnumSetType, O: Into<EnumSet<T>>> BitAnd<O> for EnumSet<T> {
    type Output = Self;
    fn bitand(self, other: O) -> Self::Output {
        self.intersection(other.into())
    }
}
impl <T : EnumSetType, O: Into<EnumSet<T>>> BitOr<O> for EnumSet<T> {
    type Output = Self;
    fn bitor(self, other: O) -> Self::Output {
        self.union(other.into())
    }
}
impl <T : EnumSetType, O: Into<EnumSet<T>>> BitXor<O> for EnumSet<T> {
    type Output = Self;
    fn bitxor(self, other: O) -> Self::Output {
        self.symmetrical_difference(other.into())
    }
}

impl <T : EnumSetType, O: Into<EnumSet<T>>> SubAssign<O> for EnumSet<T> {
    fn sub_assign(&mut self, rhs: O) {
        *self = *self - rhs;
    }
}
impl <T : EnumSetType, O: Into<EnumSet<T>>> BitAndAssign<O> for EnumSet<T> {
    fn bitand_assign(&mut self, rhs: O) {
        *self = *self & rhs;
    }
}
impl <T : EnumSetType, O: Into<EnumSet<T>>> BitOrAssign<O> for EnumSet<T> {
    fn bitor_assign(&mut self, rhs: O) {
        *self = *self | rhs;
    }
}
impl <T : EnumSetType, O: Into<EnumSet<T>>> BitXorAssign<O> for EnumSet<T> {
    fn bitxor_assign(&mut self, rhs: O) {
        *self = *self ^ rhs;
    }
}

impl <T : EnumSetType> Not for EnumSet<T> {
    type Output = Self;
    fn not(self) -> Self::Output {
        self.complement()
    }
}

impl <T : EnumSetType> From<T> for EnumSet<T> {
    fn from(t: T) -> Self {
        EnumSet::only(t)
    }
}

impl <T : EnumSetType> PartialEq<T> for EnumSet<T> {
    fn eq(&self, other: &T) -> bool {
        self.__enumset_underlying == EnumSet::<T>::mask(other.enum_into_u8())
    }
}
impl <T : EnumSetType + Debug> Debug for EnumSet<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut is_first = true;
        f.write_str("EnumSet(")?;
        for v in self.iter() {
            if !is_first { f.write_str(" | ")?; }
            is_first = false;
            v.fmt(f)?;
        }
        f.write_str(")")?;
        Ok(())
    }
}

impl <T: EnumSetType> Hash for EnumSet<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.__enumset_underlying.hash(state)
    }
}
impl <T: EnumSetType> PartialOrd for EnumSet<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.__enumset_underlying.partial_cmp(&other.__enumset_underlying)
    }
}
impl <T: EnumSetType> Ord for EnumSet<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.__enumset_underlying.cmp(&other.__enumset_underlying)
    }
}

#[cfg(feature = "serde")]
impl <T : EnumSetType> Serialize for EnumSet<T> {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        T::serialize(*self, serializer)
    }
}

#[cfg(feature = "serde")]
impl <'de, T : EnumSetType> Deserialize<'de> for EnumSet<T> {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        T::deserialize(deserializer)
    }
}

/// The iterator used by [`EnumSet`]s.
#[derive(Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Hash, Debug)]
pub struct EnumSetIter<T : EnumSetType>(EnumSet<T>, u8);
impl <T : EnumSetType> Iterator for EnumSetIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        while self.1 < EnumSet::<T>::bit_width() {
            let bit = self.1;
            self.1 += 1;
            if self.0.has_bit(bit) {
                return unsafe { Some(T::enum_from_u8(bit)) }
            }
        }
        None
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        let left_mask = !EnumSet::<T>::partial_bits(self.1);
        let left = (self.0.__enumset_underlying & left_mask).count_ones() as usize;
        (left, Some(left))
    }
}

impl<T: EnumSetType> Extend<T> for EnumSet<T> {
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        iter.into_iter().for_each(|v| { self.insert(v); });
    }
}

impl<T: EnumSetType> FromIterator<T> for EnumSet<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut set = EnumSet::default();
        set.extend(iter);
        set
    }
}

/// Creates a EnumSet literal, which can be used in const contexts.
///
/// The syntax used is `enum_set!(Type::A | Type::B | Type::C)`. Each variant must be of the same
/// type, or a error will occur at compile-time.
///
/// # Examples
///
/// ```rust
/// # use enumset::*;
/// # #[derive(EnumSetType, Debug)] enum Enum { A, B, C }
/// const CONST_SET: EnumSet<Enum> = enum_set!(Enum::A | Enum::B);
/// assert_eq!(CONST_SET, Enum::A | Enum::B);
/// ```
///
/// This macro is strongly typed. For example, the following will not compile:
///
/// ```compile_fail
/// # use enumset::*;
/// # #[derive(EnumSetType, Debug)] enum Enum { A, B, C }
/// # #[derive(EnumSetType, Debug)] enum Enum2 { A, B, C }
/// let type_error = enum_set!(Enum::A | Enum2::B);
/// ```
#[macro_export]
macro_rules! enum_set {
    () => {
        $crate::EnumSet { __enumset_underlying: 0 }
    };
    ($($value:path)|* $(|)*) => {
        $crate::internal::EnumSetSameTypeHack {
            unified: &[$($value,)*],
            enum_set: $crate::EnumSet {
                __enumset_underlying: 0 $(| (1 << ($value as u8)))*
            },
        }.enum_set
    };
}
