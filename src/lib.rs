#![cfg_attr(not(test), no_std)]
#![forbid(missing_docs)]

//! A library for defining enums that can be used in compact bit sets. It supports enums up to 128
//! variants, and has a macro to use these sets in constants.
//!
//! # Defining enums for use with EnumSet
//!
//! Enums to be used with [`EnumSet`] should be defined through the [`enum_set_type!`] macro:
//!
//! ```rust
//! # #[macro_use] extern crate enumset;
//! # use enumset::*;
//! enum_set_type! {
//!     /// Documentation for the enum
//!     pub enum Enum {
//!         A, B, C, D, E, F,
//!         #[doc(hidden)] G,
//!     }
//! }
//! # fn main() { }
//! ```
//!
//! # Working with EnumSets
//!
//! EnumSets can be constructed via [`EnumSet::new()`] like a normal set. In addition,
//! [`enum_set_type!`] creates operator overloads that allow you to create EnumSets like so:
//!
//! ```rust
//! # #[macro_use] extern crate enumset;
//! # use enumset::*;
//! # enum_set_type! {
//! #      pub enum Enum {
//! #        A, B, C, D, E, F, G,
//! #    }
//! # }
//! let new_set = Enum::A | Enum::C | Enum::G;
//! assert_eq!(new_set.len(), 3);
//! ```
//!
//! All bitwise operations you would expect to work on bitsets also work on both EnumSets and
//! enums wrapped with [`enum_set_type!`]:
//! ```
//! # #[macro_use] extern crate enumset;
//! # use enumset::*;
//! # enum_set_type! {
//! #      pub enum Enum {
//! #        A, B, C, D, E, F, G,
//! #    }
//! # }
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
//! # #[macro_use] extern crate enumset;
//! # use enumset::*;
//! # enum_set_type! {
//! #     enum Enum { A, B, C }
//! # }
//! const CONST_SET: EnumSet<Enum> = enum_set!(Enum::A | Enum::B);
//! assert_eq!(CONST_SET, Enum::A | Enum::B);
//! ```
//!
//! Mutable operations on the [`EnumSet`] otherwise work basically as expected:
//!
//! ```rust
//! # #[macro_use] extern crate enumset;
//! # use enumset::*;
//! # enum_set_type! {
//! #      pub enum Enum {
//! #        A, B, C, D, E, F, G,
//! #    }
//! # }
//! let mut set = EnumSet::new();
//! set.insert(Enum::A);
//! set.insert_all(Enum::E | Enum::G);
//! assert!(set.contains(Enum::A));
//! assert!(!set.contains(Enum::B));
//! assert_eq!(set, Enum::A | Enum::E | Enum::G);
//! ```

#[cfg(test)] extern crate core;
extern crate num_traits;

use core::cmp::Ordering;
use core::fmt;
use core::fmt::{Debug, Formatter};
use core::hash::{Hash, Hasher};
use core::ops::*;

use num_traits::*;

#[doc(hidden)]
/// The trait used to define enum types.
/// This is **NOT** public API and may change at any time.
pub unsafe trait EnumSetType: Copy {
    type Repr: PrimInt + ToPrimitive + FromPrimitive + WrappingSub + CheckedShl + Debug + Hash;
    const VARIANT_COUNT: u8;

    fn enum_into_u8(self) -> u8;
    unsafe fn enum_from_u8(val: u8) -> Self;
}

#[doc(hidden)]
/// A struct used to type check [`enum_set!`].
/// This is **NOT** public API and may change at any time.
pub struct EnumSetSameTypeHack<'a, T: EnumSetType + 'static> {
    pub unified: &'a [T],
    pub enum_set: EnumSet<T>,
}

/// An efficient set type for enums created with the [`enum_set_type!`](./macro.enum_set_type.html)
/// macro.
pub struct EnumSet<T : EnumSetType> {
    #[doc(hidden)]
    /// This is public due to the [`enum_set!`] macro.
    /// This is **NOT** public API and may change at any time.
    pub __enumset_underlying: T::Repr
}
impl <T : EnumSetType> PartialOrd for EnumSet<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.__enumset_underlying.partial_cmp(&other.__enumset_underlying)
    }
}
impl <T : EnumSetType> Ord for EnumSet<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.__enumset_underlying.cmp(&other.__enumset_underlying)
    }
}
impl <T : EnumSetType> PartialEq for EnumSet<T> {
    fn eq(&self, other: &Self) -> bool {
        self.__enumset_underlying.eq(&other.__enumset_underlying)
    }
}
impl <T : EnumSetType> Eq for EnumSet<T> { }
impl <T : EnumSetType> Hash for EnumSet<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.__enumset_underlying.hash(state)
    }
}
impl <T : EnumSetType> Clone for EnumSet<T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl <T : EnumSetType> Copy for EnumSet<T> { }
impl <T : EnumSetType> EnumSet<T> {
    fn mask(bit: u8) -> T::Repr {
        Shl::<usize>::shl(T::Repr::one(), bit as usize)
    }
    fn has_bit(&self, bit: u8) -> bool {
        let mask = Self::mask(bit);
        self.__enumset_underlying & mask == mask
    }
    fn partial_bits(bits: u8) -> T::Repr {
        assert!(bits != 0 && bits <= T::VARIANT_COUNT);
        T::Repr::one().checked_shl(bits.into())
            .unwrap_or(T::Repr::zero())
            .wrapping_sub(&T::Repr::one())
    }
    // Returns all bits valid for the enum
    fn all_bits() -> T::Repr {
        Self::partial_bits(T::VARIANT_COUNT)
    }

    /// Returns an empty set.
    pub fn new() -> Self {
        EnumSet { __enumset_underlying: T::Repr::zero() }
    }

    /// Returns a set containing a single value.
    pub fn only(t: T) -> Self {
        EnumSet { __enumset_underlying: Self::mask(t.enum_into_u8()) }
    }

    /// Returns an empty set.
    pub fn empty() -> Self {
        Self::new()
    }
    /// Returns a set with all bits set.
    pub fn all() -> Self {
        EnumSet { __enumset_underlying: Self::all_bits() }
    }

    /// Total number of bits this enumset uses. Note that the actual amount of space used is
    /// rounded up to the next highest integer type (`u8`, `u16`, `u32`, `u64`, or `u128`).
    pub fn bit_width() -> u8 {
        T::VARIANT_COUNT as u8
    }

    /// Returns the raw bits of this set
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

    /// Returns the number of values in this set.
    pub fn len(&self) -> usize {
        self.__enumset_underlying.count_ones() as usize
    }
    /// Checks if the set is empty.
    pub fn is_empty(&self) -> bool {
        self.__enumset_underlying.is_zero()
    }
    /// Removes all elements from the set.
    pub fn clear(&mut self) {
        self.__enumset_underlying = T::Repr::zero()
    }

    /// Checks if this set shares no elements with another.
    pub fn is_disjoint(&self, other: Self) -> bool {
        (*self & other).is_empty()
    }
    /// Checks if all elements in another set are in this set.
    pub fn is_superset(&self, other: Self) -> bool {
        *self & other == other
    }
    /// Checks if all elements of this set are in another set.
    pub fn is_subset(&self, other: Self) -> bool {
        other.is_superset(*self)
    }

    /// Returns a set containing the union of all elements in both sets.
    pub fn union(&self, other: Self) -> Self {
        EnumSet { __enumset_underlying: self.__enumset_underlying | other.__enumset_underlying }
    }
    /// Returns a set containing all elements in common with another set.
    pub fn intersection(&self, other: Self) -> Self {
        EnumSet { __enumset_underlying: self.__enumset_underlying & other.__enumset_underlying }
    }
    /// Returns a set with all elements of the other set removed.
    pub fn difference(&self, other: Self) -> Self {
        EnumSet { __enumset_underlying: self.__enumset_underlying & !other.__enumset_underlying }
    }
    /// Returns a set with all elements not contained in both sets.
    pub fn symmetrical_difference(&self, other: Self) -> Self {
        EnumSet { __enumset_underlying: self.__enumset_underlying ^ other.__enumset_underlying }
    }
    /// Returns a set containing all elements not in this set.
    pub fn complement(&self) -> Self {
        EnumSet { __enumset_underlying: !self.__enumset_underlying & Self::all_bits() }
    }

    /// Checks whether this set contains a value.
    pub fn contains(&self, value: T) -> bool {
        self.has_bit(value.enum_into_u8())
    }

    /// Adds a value to this set.
    pub fn insert(&mut self, value: T) -> bool {
        let contains = self.contains(value);
        self.__enumset_underlying = self.__enumset_underlying | Self::mask(value.enum_into_u8());
        contains
    }
    /// Removes a value from this set.
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
        *self == EnumSet::only(*other)
    }
}
impl <T : EnumSetType + Debug> Debug for EnumSet<T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
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

/// The iterator used by [`EnumSet`](./struct.EnumSet.html).
#[derive(Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Hash, Debug)]
pub struct EnumSetIter<T : EnumSetType>(EnumSet<T>, u8);
impl <T : EnumSetType> Iterator for EnumSetIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        while self.1 < T::VARIANT_COUNT {
            let bit = self.1;
            self.1 += 1;
            if self.0.has_bit(bit) {
                return unsafe { Some(T::enum_from_u8(bit)) }
            }
        }
        None
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        let left_mask = EnumSet::<T>::partial_bits(self.1);
        let left = (self.0.__enumset_underlying & left_mask).count_ones() as usize;
        (left, Some(left))
    }
}

#[macro_export]
#[doc(hidden)]
macro_rules! enum_set_type_internal_count_variants {
    ($next:ident ($($args:tt)*)
        $_00:ident $_01:ident $_02:ident $_03:ident $_04:ident $_05:ident $_06:ident $_07:ident
        $_10:ident $_11:ident $_12:ident $_13:ident $_14:ident $_15:ident $_16:ident $_17:ident
        $_20:ident $_21:ident $_22:ident $_23:ident $_24:ident $_25:ident $_26:ident $_27:ident
        $_30:ident $_31:ident $_32:ident $_33:ident $_34:ident $_35:ident $_36:ident $_37:ident
        $_40:ident $_41:ident $_42:ident $_43:ident $_44:ident $_45:ident $_46:ident $_47:ident
        $_50:ident $_51:ident $_52:ident $_53:ident $_54:ident $_55:ident $_56:ident $_57:ident
        $_60:ident $_61:ident $_62:ident $_63:ident $_64:ident $_65:ident $_66:ident $_67:ident
        $_70:ident $_71:ident $_72:ident $_73:ident $_74:ident $_75:ident $_76:ident $_77:ident
        $_80:ident $_81:ident $_82:ident $_83:ident $_84:ident $_85:ident $_86:ident $_87:ident
        $_90:ident $_91:ident $_92:ident $_93:ident $_94:ident $_95:ident $_96:ident $_97:ident
        $_a0:ident $_a1:ident $_a2:ident $_a3:ident $_a4:ident $_a5:ident $_a6:ident $_a7:ident
        $_b0:ident $_b1:ident $_b2:ident $_b3:ident $_b4:ident $_b5:ident $_b6:ident $_b7:ident
        $_c0:ident $_c1:ident $_c2:ident $_c3:ident $_c4:ident $_c5:ident $_c6:ident $_c7:ident
        $_d0:ident $_d1:ident $_d2:ident $_d3:ident $_d4:ident $_d5:ident $_d6:ident $_d7:ident
        $_e0:ident $_e1:ident $_e2:ident $_e3:ident $_e4:ident $_e5:ident $_e6:ident $_e7:ident
        $_f0:ident $_f1:ident $_f2:ident $_f3:ident $_f4:ident $_f5:ident $_f6:ident $_f7:ident
        $($rest:ident)+
    ) => {
        compile_error!("enum_set_type! can only accept up to 128 variants.")
    };
    ($next:ident ($($args:tt)*)
        $_00:ident $_01:ident $_02:ident $_03:ident $_04:ident $_05:ident $_06:ident $_07:ident
        $_10:ident $_11:ident $_12:ident $_13:ident $_14:ident $_15:ident $_16:ident $_17:ident
        $_20:ident $_21:ident $_22:ident $_23:ident $_24:ident $_25:ident $_26:ident $_27:ident
        $_30:ident $_31:ident $_32:ident $_33:ident $_34:ident $_35:ident $_36:ident $_37:ident
        $_40:ident $_41:ident $_42:ident $_43:ident $_44:ident $_45:ident $_46:ident $_47:ident
        $_50:ident $_51:ident $_52:ident $_53:ident $_54:ident $_55:ident $_56:ident $_57:ident
        $_60:ident $_61:ident $_62:ident $_63:ident $_64:ident $_65:ident $_66:ident $_67:ident
        $_70:ident $_71:ident $_72:ident $_73:ident $_74:ident $_75:ident $_76:ident $_77:ident
        $($rest:ident)+
    ) => {
        enum_set_type_internal! { @$next u128 $($args)* }
    };
    ($next:ident ($($args:tt)*)
        $_00:ident $_01:ident $_02:ident $_03:ident $_04:ident $_05:ident $_06:ident $_07:ident
        $_10:ident $_11:ident $_12:ident $_13:ident $_14:ident $_15:ident $_16:ident $_17:ident
        $_20:ident $_21:ident $_22:ident $_23:ident $_24:ident $_25:ident $_26:ident $_27:ident
        $_30:ident $_31:ident $_32:ident $_33:ident $_34:ident $_35:ident $_36:ident $_37:ident
        $($rest:ident)+
    ) => {
        enum_set_type_internal! { @$next u64 $($args)* }
    };
    ($next:ident ($($args:tt)*)
        $_00:ident $_01:ident $_02:ident $_03:ident $_04:ident $_05:ident $_06:ident $_07:ident
        $_10:ident $_11:ident $_12:ident $_13:ident $_14:ident $_15:ident $_16:ident $_17:ident
        $($rest:ident)+
    ) => {
        enum_set_type_internal! { @$next u32 $($args)* }
    };
    ($next:ident ($($args:tt)*)
        $_00:ident $_01:ident $_02:ident $_03:ident $_04:ident $_05:ident $_06:ident $_07:ident
        $($rest:ident)+
    ) => {
        enum_set_type_internal! { @$next u16 $($args)* }
    };
    ($next:ident ($($args:tt)*) $($rest:ident)*) => {
        enum_set_type_internal! { @$next u8 $($args)* }
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! enum_set_type_internal {
    // Counting functions
    (@ident ($($random:tt)*) $value:expr) => { $value };
    (@count $($value:tt)*) => {
        0u8 $(+ enum_set_type_internal!(@ident ($value) 1u8))*
    };

    // Codegen
    (@body $repr:ident ($(#[$enum_attr:meta])*) ($($vis:tt)*) $enum_name:ident {
        $($(#[$attr:meta])* $variant:ident,)*
    }) => {
        $(#[$enum_attr])* #[repr(u8)]
        #[derive(Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Hash, Debug)]
        $($vis)* enum $enum_name {
            $($(#[$attr])* $variant,)*
        }
        unsafe impl $crate::EnumSetType for $enum_name {
            type Repr = $repr;
            const VARIANT_COUNT: u8 = enum_set_type_internal!(@count $($variant)*);

            fn enum_into_u8(self) -> u8 {
                self as u8
            }
            unsafe fn enum_from_u8(val: u8) -> Self {
                ::std::mem::transmute(val)
            }
        }
        impl <O : Into<$crate::EnumSet<$enum_name>>> ::std::ops::Sub<O> for $enum_name {
            type Output = $crate::EnumSet<$enum_name>;
            fn sub(self, other: O) -> Self::Output {
                $crate::EnumSet::only(self) - other.into()
            }
        }
        impl <O : Into<$crate::EnumSet<$enum_name>>> ::std::ops::BitAnd<O> for $enum_name {
            type Output = $crate::EnumSet<$enum_name>;
            fn bitand(self, other: O) -> Self::Output {
                $crate::EnumSet::only(self) & other.into()
            }
        }
        impl <O : Into<$crate::EnumSet<$enum_name>>> ::std::ops::BitOr<O> for $enum_name {
            type Output = $crate::EnumSet<$enum_name>;
            fn bitor(self, other: O) -> Self::Output {
                $crate::EnumSet::only(self) | other.into()
            }
        }
        impl <O : Into<$crate::EnumSet<$enum_name>>> ::std::ops::BitXor<O> for $enum_name {
            type Output = $crate::EnumSet<$enum_name>;
            fn bitxor(self, other: O) -> Self::Output {
                $crate::EnumSet::only(self) ^ other.into()
            }
        }
        impl ::std::ops::Not for $enum_name {
            type Output = $crate::EnumSet<$enum_name>;
            fn not(self) -> Self::Output {
                !$crate::EnumSet::only(self)
            }
        }
        impl ::std::cmp::PartialEq<$crate::EnumSet<$enum_name>> for $enum_name {
            fn eq(&self, other: &$crate::EnumSet<$enum_name>) -> bool {
                $crate::EnumSet::only(*self) == *other
            }
        }
    };
}

/// Defines enums which can be used with EnumSet.
///
/// While attributes and documentation can be attached to the enum variants, the variants may not
/// contain data.
///
/// [`Copy`], [`Clone`], [`PartialOrd`], [`Ord`], [`PartialEq`], [`Eq`], [`Hash`], [`Debug`],
/// [`Sub`], [`BitAnd`], [`BitOr`], [`BitXor`], and [`Not`] are automatically derived for the enum.
///
/// These impls, in general, behave as if the enum variant was an [`EnumSet`] with a single value,
/// as those created by [`EnumSet::only`].
///
/// # Examples
///
/// ```rust
/// # #[macro_use] extern crate enumset;
/// # use enumset::*;
/// enum_set_type! {
///     enum Enum {
///         A, B, C, D, E, F, G
///     }
///
///     /// Documentation
///     pub enum Enum2 {
///         A, B, C, D, E, F, G,
///         #[doc(hidden)] __NonExhaustive,
///     }
/// }
/// # fn main() { }
/// ```
#[macro_export]
macro_rules! enum_set_type {
    ($(#[$enum_attr:meta])* $vis:vis enum $enum_name:ident {
        $($(#[$attr:meta])* $variant:ident),* $(,)*
    } $($rest:tt)*) => {
        enum_set_type_internal_count_variants!(body (($(#[$enum_attr])*) ($vis) $enum_name {
            $($(#[$attr])* $variant,)*
        }) $($variant)*);
        enum_set_type!($($rest)*);
    };
    () => { };
}

/// Creates a EnumSet literal, which can be used in const contexts.
///
/// The syntax used is `enum_set!(Type::A | Type::B | Type::C)`. Each variant must be of the same
/// type, or a error will occur at compile-time.
///
/// You may also explicitly state the type of the variants that follow, as in
/// `enum_set!(Type, Type::A | Type::B | Type::C)`.
///
/// # Examples
///
/// ```rust
/// # #[macro_use] extern crate enumset;
/// # use enumset::*;
/// # enum_set_type! {
/// #     enum Enum { A, B, C }
/// # }
/// # fn main() {
/// const CONST_SET: EnumSet<Enum> = enum_set!(Enum::A | Enum::B);
/// assert_eq!(CONST_SET, Enum::A | Enum::B);
///
/// const EXPLICIT_CONST_SET: EnumSet<Enum> = enum_set!(Enum, Enum::A | Enum::B);
/// assert_eq!(EXPLICIT_CONST_SET, Enum::A | Enum::B);
/// # }
/// ```
///
/// This macro is strongly typed. For example, the following will not compile:
///
/// ```compile_fail
/// # #[macro_use] extern crate enumset;
/// # use enumset::*;
/// # enum_set_type! {
/// #     enum Enum { A, B, C }
/// #     enum Enum2 { A, B, C }
/// # }
/// # fn main() {
/// let type_error = enum_set!(Enum::A | Enum2::B);
/// # }
/// ```
#[macro_export]
macro_rules! enum_set {
    () => {
        $crate::EnumSet { __enumset_underlying: 0 }
    };
    ($($value:path)|* $(|)*) => {
        $crate::EnumSetSameTypeHack {
            unified: &[$($value,)*],
            enum_set: $crate::EnumSet {
                __enumset_underlying: 0 $(| (1 << ($value as u8)))*
            },
        }.enum_set
    };
    ($enum_name:ty, $($value:path)|* $(|)*) => {
        $crate::EnumSet::<$enum_name> {
            __enumset_underlying: 0 $(| (1 << ($value as $enum_name as u8)))*
        }
    }
}

#[cfg(test)]
#[allow(dead_code)]
mod test {
    use super::*;

    mod enums {
        enum_set_type! {
            pub enum SmallEnum {
                A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z,
            }
            pub enum LargeEnum {
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
            pub enum Enum8 {
                A, B, C, D, E, F, G, H,
            }
            pub enum Enum128 {
                A, B, C, D, E, F, G, H, _8, _9, _10, _11, _12, _13, _14, _15,
                _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31,
                _32, _33, _34, _35, _36, _37, _38, _39, _40, _41, _42, _43, _44, _45, _46, _47,
                _48, _49, _50, _51, _52, _53, _54, _55, _56, _57, _58, _59, _60, _61, _62, _63,
                _64, _65, _66, _67, _68, _69, _70, _71, _72, _73, _74, _75, _76, _77, _78, _79,
                _80, _81, _82, _83, _84, _85, _86, _87, _88, _89, _90, _91, _92, _93, _94, _95,
                _96, _97, _98, _99, _100, _101, _102, _103, _104, _105, _106, _107, _108, _109,
                _110, _111, _112, _113, _114, _115, _116, _117, _118, _119, _120, _121, _122, _123,
                _124,  _125, _126, _127,
            }
        }
    }
    use self::enums::*;

    macro_rules! test_variants {
        ($enum_name:ident $variant_range:ident $all_empty_test:ident $($variant:ident,)*) => {
            #[test]
            fn $variant_range() {
                let count = enum_set_type_internal!(@count u8 $($variant)*);
                $(
                    assert!(($enum_name::$variant as u8) < count);
                )*
            }

            #[test]
            fn $all_empty_test() {
                let all = EnumSet::<$enum_name>::all();
                let empty = EnumSet::<$enum_name>::empty();

                $(
                    assert!(!empty.contains($enum_name::$variant));
                    assert!(all.contains($enum_name::$variant));
                )*
            }
        }
    }

    test_variants! { SmallEnum enum_variant_range_test enum_all_empty
        A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z,
    }

    test_variants! { LargeEnum large_enum_variant_range_test large_enum_all_empty
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

    macro_rules! test_enum {
        ($e:ident, $m:ident) => {
            mod $m {
                use super::*;

                const CONST_SET: EnumSet<$e> = enum_set!($e, $e::A | $e::C);
                const EMPTY_SET: EnumSet<$e> = enum_set!();
                #[test]
                fn const_set() {
                    assert_eq!(CONST_SET.len(), 2);
                    assert!(CONST_SET.contains($e::A));
                    assert!(CONST_SET.contains($e::C));
                    assert!(EMPTY_SET.is_empty());
                }

                #[test]
                fn basic_add_remove() {
                    let mut set = EnumSet::new();
                    set.insert($e::A);
                    set.insert($e::B);
                    set.insert($e::C);
                    assert_eq!(set, $e::A | $e::B | $e::C);
                    set.remove($e::B);
                    assert_eq!(set, $e::A | $e::C);
                    set.insert($e::D);
                    assert_eq!(set, $e::A | $e::C | $e::D);
                    set.insert_all($e::F | $e::E | $e::G);
                    assert_eq!(set, $e::A | $e::C | $e::D | $e::F | $e::E | $e::G);
                    set.remove_all($e::A | $e::D | $e::G);
                    assert_eq!(set, $e::C | $e::F | $e::E);
                    assert!(!set.is_empty());
                    set.clear();
                    assert!(set.is_empty());
                }

                #[test]
                fn empty_is_empty() {
                    assert_eq!(EnumSet::<$e>::empty().len(), 0)
                }

                #[test]
                fn all_len() {
                    assert_eq!(EnumSet::<$e>::all().len(), EnumSet::<$e>::bit_width() as usize)
                }

                #[test]
                fn basic_iter_test() {
                    let mut set = EnumSet::new();
                    set.insert($e::A);
                    set.insert($e::B);
                    set.insert($e::C);
                    set.insert($e::E);

                    let mut set_2 = EnumSet::new();
                    let vec: Vec<$e> = set.iter().collect();
                    for val in vec {
                        assert!(!set_2.contains(val));
                        set_2.insert(val);
                    }
                    assert_eq!(set, set_2);

                    let mut set_3 = EnumSet::new();
                    for val in set {
                        assert!(!set_3.contains(val));
                        set_3.insert(val);
                    }
                    assert_eq!(set, set_3);
                }

                #[test]
                fn basic_ops_test() {
                    assert_eq!(($e::A | $e::B) | ($e::B | $e::C), $e::A | $e::B | $e::C);
                    assert_eq!(($e::A | $e::B) & ($e::B | $e::C), $e::B);
                    assert_eq!(($e::A | $e::B) ^ ($e::B | $e::C), $e::A | $e::C);
                    assert_eq!(($e::A | $e::B) - ($e::B | $e::C), $e::A);
                }

                #[test]
                fn basic_set_status() {
                    assert!(($e::A | $e::B | $e::C).is_disjoint($e::D | $e::E | $e::F));
                    assert!(!($e::A | $e::B | $e::C | $e::D).is_disjoint($e::D | $e::E | $e::F));
                    assert!(($e::A | $e::B).is_subset($e::A | $e::B | $e::C));
                    assert!(!($e::A | $e::D).is_subset($e::A | $e::B | $e::C));
                }

                #[test]
                fn debug_impl() {
                    assert_eq!(format!("{:?}", $e::A | $e::B | $e::D), "EnumSet(A | B | D)");
                }

                #[test]
                fn to_from_bits() {
                    let value = $e::A | $e::C | $e::D | $e::F | $e::E | $e::G;
                    assert_eq!(EnumSet::from_bits(value.to_bits()), value);
                }

                #[test]
                #[should_panic]
                fn too_many_bits() {
                    if EnumSet::<$e>::bit_width() == 128 {
                        panic!("(test skipped)")
                    }
                    EnumSet::<$e>::from_bits(!0);
                }
            }
        }
    }

    test_enum!(SmallEnum, small_enum);
    test_enum!(LargeEnum, large_enum);
    test_enum!(Enum8, enum8);
    test_enum!(Enum128, enum128);
}
