use crate::repr::EnumSetTypeRepr;
use crate::traits::EnumSetType;
use core::cmp::Ordering;
use core::fmt::{Debug, Formatter};
use core::hash::{Hash, Hasher};
use core::ops::{
    BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Not, Sub, SubAssign,
};

#[cfg(feature = "serde")]
use {
    serde2 as serde,
    serde2::{Deserialize, Serialize},
};

/// An efficient set type for enums.
///
/// It is implemented using a bitset stored using the smallest integer that can fit all bits
/// in the underlying enum. In general, an enum variant with a discriminator of `n` is stored in
/// the nth least significant bit (corresponding with a mask of, e.g. `1 << enum as u32`).
///
/// # Numeric representation
///
/// `EnumSet` is internally implemented using integer types, and as such can be easily converted
/// from and to numbers.
///
/// Each bit of the underlying integer corresponds to at most one particular enum variant. If the
/// corresponding bit for a variant is set, it present in the set. Bits that do not correspond to
/// any variant are always unset.
///
/// By default, each enum variant is stored in a bit corresponding to its discriminator. An enum
/// variant with a discriminator of `n` is stored in the `n + 1`th least significant bit
/// (corresponding to a mask of e.g. `1 << enum as u32`).
///
/// # Array representation
///
/// Sets with more than 128 variants are instead stored with an underlying array of `u64`s. This
/// is treated as if it was a single large integer. The `n`th least significant bit of this integer
/// is stored in the `n % 64`th least significant bit of the `n / 64`th element in the array.
///
/// # Serialization
///
/// When the `serde` feature is enabled, `EnumSet`s can be serialized and deserialized using
/// the `serde` crate. The exact serialization format can be controlled with additional attributes
/// on the enum type. These attributes are valid regardless of whether the `serde` feature
/// is enabled.
///
/// By default, `EnumSet` is serialized by directly writing out a single integer containing the
/// numeric representation of the bitset. The integer type used is the smallest one that can fit
/// the largest variant in the enum. If no integer type is large enough, instead the `EnumSet` is
/// serialized as an array of `u64`s containing the array representation.
///
/// The `#[enumset(serialize_repr = "…")]` attribute can be used to override the representation
/// used. Valid values are as follows:
///
/// * `u8`, `u16`, `u32`, `u64`, and `u128` serialize the type as the corresponding integer type.
/// * `array` serializes the set as an list of `u64`s corresponding to the array representation.
/// * `list` serializes the set as a list of enum variants. This requires your enum type implement
///   [`Serialize`] and [`Deserialize`].
/// * `map` serializes the set as a map of enum variants to booleans. The set contains a value if
///   the boolean is `true`. This requires your enum type implement `Serialize` and `Deserialize`.
///
/// The representation used is determined statically at compile time, and there is currently no
/// support for reading different formats with the same deserializer.
///
/// By default, unknown bits are ignored and silently removed from the bitset. To override this
/// behavior, you can add a `#[enumset(serialize_deny_unknown)]` attribute. This will cause
/// deserialization to fail if an invalid bit is set.
///
/// # FFI, Safety and `repr`
///
/// If an enum type `T` is annotated with
/// [`#[enumset(repr = "…")]`](derive@crate::EnumSetType#options) where `…` is a primitive integer
/// type, then several things happen:
///
/// * `T` will implement
///   <code>[EnumSetTypeWithRepr](crate::traits::EnumSetTypeWithRepr)&lt;Repr = R&gt;</code> in
///   addition to [`EnumSetType`].
/// * The `EnumSet` methods with `repr` in their name, such as [`as_repr`][EnumSet::as_repr] and
///   [`from_repr`][EnumSet::from_repr], will be available for `EnumSet<T>`.
/// * The in-memory representation of `EnumSet<T>` is guaranteed to be `R`.
///
/// That last guarantee makes it sound to send `EnumSet<T>` across an FFI boundary. For example:
///
/// ```
/// # use enumset::*;
/// #
/// # mod ffi_impl {
/// #     // This example “foreign” function is actually written in Rust, but for the sake
/// #     // of example, we'll pretend it's written in C.
/// #     #[no_mangle]
/// #     extern "C" fn some_foreign_function(set: u32) -> u32 {
/// #         set & 0b100
/// #     }
/// # }
/// #
/// extern "C" {
///     // This function is written in C like:
///     // uint32_t some_foreign_function(uint32_t set) { … }
///     fn some_foreign_function(set: EnumSet<MyEnum>) -> EnumSet<MyEnum>;
/// }
///
/// #[derive(Debug, EnumSetType)]
/// #[enumset(repr = "u32")]
/// enum MyEnum { A, B, C }
///
/// let set: EnumSet<MyEnum> = enum_set!(MyEnum::A | MyEnum::C);
///
/// let new_set: EnumSet<MyEnum> = unsafe { some_foreign_function(set) };
/// assert_eq!(new_set, enum_set!(MyEnum::C));
/// ```
///
/// When an `EnumSet<T>` is received via FFI, all bits that don't correspond to an enum variant
/// of `T` must be set to `0`. Behavior is **undefined** if any of these bits are set to `1`.
#[cfg_attr(
    not(feature = "serde"),
    doc = "\n\n",
    doc = "[`Serialize`]: https://docs.rs/serde/latest/serde/trait.Serialize.html\n",
    doc = "[`Deserialize`]: https://docs.rs/serde/latest/serde/trait.Deserialize.html\n"
)]
#[derive(Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct EnumSet<T: EnumSetType> {
    #[doc(hidden)]
    /// This is public due to the `enum_set!` macro.
    /// This is **NOT** public API and may change at any time.
    pub __priv_repr: T::Repr,
}
impl<T: EnumSetType> EnumSet<T> {
    /// An empty `EnumSet`.
    ///
    /// This is available as a constant for use in constant expressions.
    pub const EMPTY: Self = EnumSet { __priv_repr: T::Repr::EMPTY };

    /// An `EnumSet` containing all valid variants of the enum.
    ///
    /// This is available as a constant for use in constant expressions.
    pub const ALL: Self = EnumSet { __priv_repr: T::ALL_BITS };

    /// Creates an empty `EnumSet`.
    #[inline(always)]
    pub fn new() -> Self {
        Self::EMPTY
    }

    /// Returns an `EnumSet` containing a single element.
    #[inline(always)]
    pub fn only(t: T) -> Self {
        let mut set = Self::new();
        set.insert(t);
        set
    }

    /// Creates an empty `EnumSet`.
    ///
    /// This is an alias for [`EnumSet::new`].
    #[inline(always)]
    pub fn empty() -> Self {
        Self::EMPTY
    }

    /// Returns an `EnumSet` containing all valid variants of the enum.
    #[inline(always)]
    pub fn all() -> Self {
        Self::ALL
    }

    /// Total number of bits used by this type. Note that the actual amount of space used is
    /// rounded up to the next highest integer type (`u8`, `u16`, `u32`, `u64`, or `u128`).
    ///
    /// This is the same as [`EnumSet::variant_count`] except in enums with "sparse" variants.
    /// (e.g. `enum Foo { A = 10, B = 20 }`)
    #[inline(always)]
    pub fn bit_width() -> u32 {
        T::BIT_WIDTH
    }

    /// The number of valid variants that this type can contain.
    ///
    /// This is the same as [`EnumSet::bit_width`] except in enums with "sparse" variants.
    /// (e.g. `enum Foo { A = 10, B = 20 }`)
    #[inline(always)]
    pub fn variant_count() -> u32 {
        T::VARIANT_COUNT
    }

    /// Returns the number of elements in this set.
    #[inline(always)]
    pub fn len(&self) -> usize {
        self.__priv_repr.count_ones() as usize
    }
    /// Returns `true` if the set contains no elements.
    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.__priv_repr.is_empty()
    }
    /// Removes all elements from the set.
    #[inline(always)]
    pub fn clear(&mut self) {
        self.__priv_repr = T::Repr::EMPTY;
    }

    /// Returns `true` if `self` has no elements in common with `other`. This is equivalent to
    /// checking for an empty intersection.
    #[inline(always)]
    pub fn is_disjoint(&self, other: Self) -> bool {
        (*self & other).is_empty()
    }
    /// Returns `true` if the set is a superset of another, i.e., `self` contains at least all the
    /// values in `other`.
    #[inline(always)]
    pub fn is_superset(&self, other: Self) -> bool {
        (*self & other).__priv_repr == other.__priv_repr
    }
    /// Returns `true` if the set is a subset of another, i.e., `other` contains at least all
    /// the values in `self`.
    #[inline(always)]
    pub fn is_subset(&self, other: Self) -> bool {
        other.is_superset(*self)
    }

    /// Returns a set containing any elements present in either set.
    #[inline(always)]
    pub fn union(&self, other: Self) -> Self {
        EnumSet { __priv_repr: self.__priv_repr | other.__priv_repr }
    }
    /// Returns a set containing every element present in both sets.
    #[inline(always)]
    pub fn intersection(&self, other: Self) -> Self {
        EnumSet { __priv_repr: self.__priv_repr & other.__priv_repr }
    }
    /// Returns a set containing element present in `self` but not in `other`.
    #[inline(always)]
    pub fn difference(&self, other: Self) -> Self {
        EnumSet { __priv_repr: self.__priv_repr.and_not(other.__priv_repr) }
    }
    /// Returns a set containing every element present in either `self` or `other`, but is not
    /// present in both.
    #[inline(always)]
    pub fn symmetrical_difference(&self, other: Self) -> Self {
        EnumSet { __priv_repr: self.__priv_repr ^ other.__priv_repr }
    }
    /// Returns a set containing all enum variants not in this set.
    #[inline(always)]
    pub fn complement(&self) -> Self {
        EnumSet { __priv_repr: !self.__priv_repr & T::ALL_BITS }
    }

    /// Checks whether this set contains a value.
    #[inline(always)]
    pub fn contains(&self, value: T) -> bool {
        self.__priv_repr.has_bit(value.enum_into_u32())
    }

    /// Adds a value to this set.
    ///
    /// If the set did not have this value present, `true` is returned.
    ///
    /// If the set did have this value present, `false` is returned.
    #[inline(always)]
    pub fn insert(&mut self, value: T) -> bool {
        let contains = !self.contains(value);
        self.__priv_repr.add_bit(value.enum_into_u32());
        contains
    }
    /// Removes a value from this set. Returns whether the value was present in the set.
    #[inline(always)]
    pub fn remove(&mut self, value: T) -> bool {
        let contains = self.contains(value);
        self.__priv_repr.remove_bit(value.enum_into_u32());
        contains
    }

    /// Adds all elements in another set to this one.
    #[inline(always)]
    pub fn insert_all(&mut self, other: Self) {
        self.__priv_repr = self.__priv_repr | other.__priv_repr
    }
    /// Removes all values in another set from this one.
    #[inline(always)]
    pub fn remove_all(&mut self, other: Self) {
        self.__priv_repr = self.__priv_repr.and_not(other.__priv_repr);
    }
}

impl<T: EnumSetType> Default for EnumSet<T> {
    /// Returns an empty set.
    fn default() -> Self {
        Self::new()
    }
}

impl<T: EnumSetType, O: Into<EnumSet<T>>> Sub<O> for EnumSet<T> {
    type Output = Self;
    #[inline(always)]
    fn sub(self, other: O) -> Self::Output {
        self.difference(other.into())
    }
}
impl<T: EnumSetType, O: Into<EnumSet<T>>> BitAnd<O> for EnumSet<T> {
    type Output = Self;
    #[inline(always)]
    fn bitand(self, other: O) -> Self::Output {
        self.intersection(other.into())
    }
}
impl<T: EnumSetType, O: Into<EnumSet<T>>> BitOr<O> for EnumSet<T> {
    type Output = Self;
    #[inline(always)]
    fn bitor(self, other: O) -> Self::Output {
        self.union(other.into())
    }
}
impl<T: EnumSetType, O: Into<EnumSet<T>>> BitXor<O> for EnumSet<T> {
    type Output = Self;
    #[inline(always)]
    fn bitxor(self, other: O) -> Self::Output {
        self.symmetrical_difference(other.into())
    }
}

impl<T: EnumSetType, O: Into<EnumSet<T>>> SubAssign<O> for EnumSet<T> {
    #[inline(always)]
    fn sub_assign(&mut self, rhs: O) {
        *self = *self - rhs;
    }
}
impl<T: EnumSetType, O: Into<EnumSet<T>>> BitAndAssign<O> for EnumSet<T> {
    #[inline(always)]
    fn bitand_assign(&mut self, rhs: O) {
        *self = *self & rhs;
    }
}
impl<T: EnumSetType, O: Into<EnumSet<T>>> BitOrAssign<O> for EnumSet<T> {
    #[inline(always)]
    fn bitor_assign(&mut self, rhs: O) {
        *self = *self | rhs;
    }
}
impl<T: EnumSetType, O: Into<EnumSet<T>>> BitXorAssign<O> for EnumSet<T> {
    #[inline(always)]
    fn bitxor_assign(&mut self, rhs: O) {
        *self = *self ^ rhs;
    }
}

impl<T: EnumSetType> Not for EnumSet<T> {
    type Output = Self;
    #[inline(always)]
    fn not(self) -> Self::Output {
        self.complement()
    }
}

impl<T: EnumSetType> From<T> for EnumSet<T> {
    fn from(t: T) -> Self {
        EnumSet::only(t)
    }
}

impl<T: EnumSetType> PartialEq<T> for EnumSet<T> {
    fn eq(&self, other: &T) -> bool {
        self.__priv_repr == EnumSet::only(*other).__priv_repr
    }
}
impl<T: EnumSetType + Debug> Debug for EnumSet<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        let mut is_first = true;
        f.write_str("EnumSet(")?;
        for v in self.iter() {
            if !is_first {
                f.write_str(" | ")?;
            }
            is_first = false;
            v.fmt(f)?;
        }
        f.write_str(")")?;
        Ok(())
    }
}

#[allow(clippy::derived_hash_with_manual_eq)] // This impl exists to change trait bounds only.
impl<T: EnumSetType> Hash for EnumSet<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.__priv_repr.hash(state)
    }
}
impl<T: EnumSetType> PartialOrd for EnumSet<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.__priv_repr.partial_cmp(&other.__priv_repr)
    }
}
impl<T: EnumSetType> Ord for EnumSet<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.__priv_repr.cmp(&other.__priv_repr)
    }
}

#[cfg(feature = "serde")]
impl<T: EnumSetType> Serialize for EnumSet<T> {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        T::serialize(*self, serializer)
    }
}

#[cfg(feature = "serde")]
impl<'de, T: EnumSetType> Deserialize<'de> for EnumSet<T> {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        T::deserialize(deserializer)
    }
}

// order is intentional for documentation purposes.
mod iter;
pub use iter::EnumSetIter;

mod conversion;
