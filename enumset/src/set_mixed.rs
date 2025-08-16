use crate::repr::EnumSetTypeRepr;
use crate::traits::{EnumSetConstHelper, EnumSetType, EnumSetTypePrivate};
use crate::EnumSetTypeWithRepr;
use core::cmp::Ordering;
use core::fmt::{Debug, Display, Formatter};
use core::hash::{Hash, Hasher};
use core::iter::Sum;
use core::ops::{
    BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Not, Sub, SubAssign,
};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// A variant of [`EnumSet`] that preserves unknown bits.
///
/// It only works for enums with an
/// [`#[enumset(repr = "…")]`](derive@crate::EnumSetType#representation-options) attribute used
/// with a primitive integer type.
///
/// # Numeric Representation
///
/// `MixedEnumSet` uses the same underlying
/// [`numeric representation`](EnumSet#numeric-representation) as `EnumSet`. However, bits that do
/// not correspond to an enum variant can be set.
///
/// # Serialization
///
/// When the `serde` feature is enabled, `MixedEnumSet`s can be serialized and deserialized using
/// the `serde` crate. It always serialized as a single integer of the underlying repr type.
///
/// Unlike `EnumSet`, it ignores all flags given to [`EnumSetType`](derive@crate::EnumSetType).
///
/// # FFI Safety
///
/// `MixedEnumSet` may be used interchangeably with the specified repr type in FFI.
#[derive(Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct MixedEnumSet<T: EnumSetTypeWithRepr> {
    __priv_repr: <T as EnumSetTypePrivate>::Repr,
}

//region MixedEnumSet operations
impl<T: EnumSetTypeWithRepr> MixedEnumSet<T> {
    const EMPTY_REPR: Self = MixedEnumSet { __priv_repr: <T as EnumSetTypePrivate>::Repr::EMPTY };
    const ALL_REPR: Self = MixedEnumSet { __priv_repr: T::ALL_BITS };

    /// Creates an empty `MixedEnumSet`.
    #[inline(always)]
    pub const fn new() -> Self {
        Self::EMPTY_REPR
    }

    /// Creates an empty `MixedEnumSet`.
    ///
    /// This is an alias for [`MixedEnumSet::new`].
    #[inline(always)]
    pub const fn empty() -> Self {
        Self::EMPTY_REPR
    }

    /// Returns an `MixedEnumSet` containing all valid variants of the enum.
    #[inline(always)]
    pub const fn all() -> Self {
        Self::ALL_REPR
    }

    /// The number of valid variants that this type can contain.
    #[inline(always)]
    pub const fn variant_count() -> u32 {
        T::VARIANT_COUNT
    }

    set_common_impls!(T, T::Repr);

    /// Returns the number of elements in this set, excluding unknown bits.
    #[inline(always)]
    pub fn valid_len(&self) -> usize {
        (self.__priv_repr & T::ALL_BITS).count_ones() as usize
    }
}

/// A helper type used for constant evaluation of enum operations.
#[doc(hidden)]
pub struct EnumSetInitHelper;
impl EnumSetInitHelper {
    /// Just returns this value - the version for the enums themselves would wrap it into an
    /// enumset.
    pub const fn const_only<T>(&self, value: T) -> T {
        value
    }
}

#[doc(hidden)]
unsafe impl<T: EnumSetType> EnumSetConstHelper for EnumSet<T> {
    type ConstInitHelper = EnumSetInitHelper;
    const CONST_INIT_HELPER: Self::ConstInitHelper = EnumSetInitHelper;

    type ConstOpHelper = T::ConstOpHelper;
    const CONST_OP_HELPER: Self::ConstOpHelper = T::CONST_OP_HELPER;
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
        // Note: We don't use `.debug_struct` to avoid splitting lines when using `{:x}`
        f.write_str("EnumSet(")?;
        let mut i = self.iter();
        if let Some(v) = i.next() {
            v.fmt(f)?;
            for v in i {
                f.write_str(" | ")?;
                v.fmt(f)?;
            }
        }
        f.write_str(")")?;
        Ok(())
    }
}
impl<T: EnumSetType + Display> Display for EnumSet<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        let mut i = self.iter();
        if let Some(v) = i.next() {
            v.fmt(f)?;
            for v in i {
                f.write_str(" | ")?;
                v.fmt(f)?;
            }
        }
        Ok(())
    }
}

#[cfg(feature = "defmt")]
impl<T: EnumSetType + defmt::Format> defmt::Format for EnumSet<T> {
    fn format(&self, f: defmt::Formatter) {
        let mut i = self.iter();
        if let Some(v) = i.next() {
            defmt::write!(f, "{}", v);
            for v in i {
                defmt::write!(f, " | {}", v);
            }
        }
    }
}

#[allow(clippy::derived_hash_with_manual_eq)] // This impl exists to change trait bounds only.
impl<T: EnumSetType> Hash for EnumSet<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.__priv_repr.hash(state)
    }
}
#[allow(clippy::non_canonical_partial_ord_impl)]
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
//endregion

//region EnumSet conversions
impl<T: EnumSetType + EnumSetTypeWithRepr> EnumSet<T> {
    /// Returns a `T::Repr` representing the elements of this set.
    ///
    /// Unlike the other `as_*` methods, this method is zero-cost and guaranteed not to fail,
    /// panic or truncate any bits.
    ///
    /// In order to use this method, the definition of `T` must have an
    /// [`#[enumset(repr = "…")]`](derive@crate::EnumSetType#representation-options) annotation
    /// with a primitive integer type.
    #[inline(always)]
    pub const fn as_repr(&self) -> <T as EnumSetTypeWithRepr>::Repr {
        self.__priv_repr
    }

    /// Constructs a bitset from a `T::Repr` without checking for invalid bits.
    ///
    /// Unlike the other `from_*` methods, this method is zero-cost and guaranteed not to fail,
    /// panic or truncate any bits, provided the conditions under “Safety” are upheld.
    ///
    /// In order to use this method, the definition of `T` must have an
    /// [`#[enumset(repr = "…")]`](derive@crate::EnumSetType#representation-options) annotation
    /// with a primitive integer type.
    ///
    /// # Safety
    ///
    /// All bits in the provided parameter `bits` that don't correspond to an enum variant of
    /// `T` must be set to `0`. Behavior is **undefined** if any of these bits are set to `1`.
    #[inline(always)]
    pub unsafe fn from_repr_unchecked(bits: <T as EnumSetTypeWithRepr>::Repr) -> Self {
        Self { __priv_repr: bits }
    }

    /// Constructs a bitset from a `T::Repr`.
    ///
    /// If a bit that doesn't correspond to an enum variant is set, this
    /// method will panic.
    ///
    /// In order to use this method, the definition of `T` must have an
    /// [`#[enumset(repr = "…")]`](derive@crate::EnumSetType#representation-options) annotation
    /// with a primitive integer type.
    #[inline(always)]
    pub fn from_repr(bits: <T as EnumSetTypeWithRepr>::Repr) -> Self {
        Self::try_from_repr(bits).expect("Bitset contains invalid variants.")
    }

    /// Attempts to constructs a bitset from a `T::Repr`.
    ///
    /// If a bit that doesn't correspond to an enum variant is set, this
    /// method will return `None`.
    ///
    /// In order to use this method, the definition of `T` must have an
    /// [`#[enumset(repr = "…")]`](derive@crate::EnumSetType#representation-options) annotation
    /// with a primitive integer type.
    #[inline(always)]
    pub fn try_from_repr(bits: <T as EnumSetTypeWithRepr>::Repr) -> Option<Self> {
        let mask = Self::all().__priv_repr;
        if bits.and_not(mask).is_empty() {
            Some(EnumSet { __priv_repr: bits })
        } else {
            None
        }
    }

    /// Constructs a bitset from a `T::Repr`, ignoring invalid variants.
    ///
    /// In order to use this method, the definition of `T` must have an
    /// [`#[enumset(repr = "…")]`](derive@crate::EnumSetType#representation-options) annotation
    /// with a primitive integer type.
    #[inline(always)]
    pub fn from_repr_truncated(bits: <T as EnumSetTypeWithRepr>::Repr) -> Self {
        let mask = Self::all().as_repr();
        let bits = bits & mask;
        EnumSet { __priv_repr: bits }
    }
}

/// Helper macro for generating conversion functions.
macro_rules! conversion_impls {
    (
        $(for_num!(
            $underlying:ty, $underlying_str:expr,
            $from_fn:ident $to_fn:ident $from_fn_opt:ident $to_fn_opt:ident,
            $from:ident $try_from:ident $from_truncated:ident $from_unchecked:ident,
            $to:ident $try_to:ident $to_truncated:ident
        );)*
    ) => {
        impl<T: EnumSetType> EnumSet<T> {$(
            #[doc = "Returns a `"]
            #[doc = $underlying_str]
            #[doc = "` representing the elements of this set.\n\nIf the underlying bitset will \
                     not fit in a `"]
            #[doc = $underlying_str]
            #[doc = "`, this method will panic."]
            #[inline(always)]
            pub fn $to(&self) -> $underlying {
                self.$try_to().expect("Bitset will not fit into this type.")
            }

            #[doc = "Tries to return a `"]
            #[doc = $underlying_str]
            #[doc = "` representing the elements of this set.\n\nIf the underlying bitset will \
                     not fit in a `"]
            #[doc = $underlying_str]
            #[doc = "`, this method will panic."]
            #[inline(always)]
            pub fn $try_to(&self) -> Option<$underlying> {
                EnumSetTypeRepr::$to_fn_opt(&self.__priv_repr)
            }

            #[doc = "Returns a truncated `"]
            #[doc = $underlying_str]
            #[doc = "` representing the elements of this set.\n\nIf the underlying bitset will \
                     not fit in a `"]
            #[doc = $underlying_str]
            #[doc = "`, this method will truncate any bits that don't fit."]
            #[inline(always)]
            pub fn $to_truncated(&self) -> $underlying {
                EnumSetTypeRepr::$to_fn(&self.__priv_repr)
            }

            #[doc = "Constructs a bitset from a `"]
            #[doc = $underlying_str]
            #[doc = "`.\n\nIf a bit that doesn't correspond to an enum variant is set, this \
                     method will panic."]
            #[inline(always)]
            pub fn $from(bits: $underlying) -> Self {
                Self::$try_from(bits).expect("Bitset contains invalid variants.")
            }

            #[doc = "Attempts to constructs a bitset from a `"]
            #[doc = $underlying_str]
            #[doc = "`.\n\nIf a bit that doesn't correspond to an enum variant is set, this \
                     method will return `None`."]
            #[inline(always)]
            pub fn $try_from(bits: $underlying) -> Option<Self> {
                let bits = T::Repr::$from_fn_opt(bits);
                let mask = T::ALL_BITS;
                bits.and_then(|bits| if bits.and_not(mask).is_empty() {
                    Some(EnumSet { __priv_repr: bits })
                } else {
                    None
                })
            }

            #[doc = "Constructs a bitset from a `"]
            #[doc = $underlying_str]
            #[doc = "`, ignoring bits that do not correspond to a variant."]
            #[inline(always)]
            pub fn $from_truncated(bits: $underlying) -> Self {
                let mask = Self::all().$to_truncated();
                let bits = <T::Repr as EnumSetTypeRepr>::$from_fn(bits & mask);
                EnumSet { __priv_repr: bits }
            }

            #[doc = "Constructs a bitset from a `"]
            #[doc = $underlying_str]
            #[doc = "`, without checking for invalid bits."]
            ///
            /// # Safety
            ///
            /// All bits in the provided parameter `bits` that don't correspond to an enum variant
            /// of `T` must be set to `0`. Behavior is **undefined** if any of these bits are set
            /// to `1`.
            #[inline(always)]
            pub unsafe fn $from_unchecked(bits: $underlying) -> Self {
                EnumSet { __priv_repr: <T::Repr as EnumSetTypeRepr>::$from_fn(bits) }
            }
        )*}
    }
}
conversion_impls! {
    for_num!(u8, "u8",
             from_u8 to_u8 from_u8_opt to_u8_opt,
             from_u8 try_from_u8 from_u8_truncated from_u8_unchecked,
             as_u8 try_as_u8 as_u8_truncated);
    for_num!(u16, "u16",
             from_u16 to_u16 from_u16_opt to_u16_opt,
             from_u16 try_from_u16 from_u16_truncated from_u16_unchecked,
             as_u16 try_as_u16 as_u16_truncated);
    for_num!(u32, "u32",
             from_u32 to_u32 from_u32_opt to_u32_opt,
             from_u32 try_from_u32 from_u32_truncated from_u32_unchecked,
             as_u32 try_as_u32 as_u32_truncated);
    for_num!(u64, "u64",
             from_u64 to_u64 from_u64_opt to_u64_opt,
             from_u64 try_from_u64 from_u64_truncated from_u64_unchecked,
             as_u64 try_as_u64 as_u64_truncated);
    for_num!(u128, "u128",
             from_u128 to_u128 from_u128_opt to_u128_opt,
             from_u128 try_from_u128 from_u128_truncated from_u128_unchecked,
             as_u128 try_as_u128 as_u128_truncated);
    for_num!(usize, "usize",
             from_usize to_usize from_usize_opt to_usize_opt,
             from_usize try_from_usize from_usize_truncated from_usize_unchecked,
             as_usize try_as_usize as_usize_truncated);
}

impl<T: EnumSetType> EnumSet<T> {
    /// Returns an `[u64; O]` representing the elements of this set.
    ///
    /// If the underlying bitset will not fit in a `[u64; O]`, this method will panic.
    pub fn as_array<const O: usize>(&self) -> [u64; O] {
        self.try_as_array()
            .expect("Bitset will not fit into this type.")
    }

    /// Returns an `[u64; O]` representing the elements of this set.
    ///
    /// If the underlying bitset will not fit in a `[u64; O]`, this method will instead return
    /// `None`.
    pub fn try_as_array<const O: usize>(&self) -> Option<[u64; O]> {
        self.__priv_repr.to_u64_array_opt()
    }

    /// Returns an `[u64; O]` representing the elements of this set.
    ///
    /// If the underlying bitset will not fit in a `[u64; O]`, this method will truncate any bits
    /// that don't fit.
    pub fn as_array_truncated<const O: usize>(&self) -> [u64; O] {
        self.__priv_repr.to_u64_array()
    }

    /// Attempts to constructs a bitset from a `[u64; O]`.
    ///
    /// If a bit that doesn't correspond to an enum variant is set, this method will panic.
    pub fn from_array<const O: usize>(v: [u64; O]) -> Self {
        Self::try_from_array(v).expect("Bitset contains invalid variants.")
    }

    /// Attempts to constructs a bitset from a `[u64; O]`.
    ///
    /// If a bit that doesn't correspond to an enum variant is set, this method will return `None`.
    pub fn try_from_array<const O: usize>(bits: [u64; O]) -> Option<Self> {
        let bits = T::Repr::from_u64_array_opt::<O>(bits);
        let mask = T::ALL_BITS;
        bits.and_then(|bits| {
            if bits.and_not(mask).is_empty() {
                Some(EnumSet { __priv_repr: bits })
            } else {
                None
            }
        })
    }

    /// Constructs a bitset from a `[u64; O]`, ignoring bits that do not correspond to a variant.
    pub fn from_array_truncated<const O: usize>(bits: [u64; O]) -> Self {
        let bits = T::Repr::from_u64_array(bits) & T::ALL_BITS;
        EnumSet { __priv_repr: bits }
    }

    /// Constructs a bitset from a `[u64; O]`, without checking for invalid bits.
    ///
    /// # Safety
    ///
    /// All bits in the provided parameter `bits` that don't correspond to an enum variant
    /// of `T` must be set to `0`. Behavior is **undefined** if any of these bits are set
    /// to `1`.
    #[inline(always)]
    pub unsafe fn from_array_unchecked<const O: usize>(bits: [u64; O]) -> Self {
        EnumSet { __priv_repr: T::Repr::from_u64_array(bits) }
    }

    /// Returns a `Vec<u64>` representing the elements of this set.
    #[cfg(feature = "alloc")]
    #[cfg_attr(docsrs, doc(cfg(feature = "alloc")))]
    pub fn to_vec(&self) -> alloc::vec::Vec<u64> {
        let mut vec = alloc::vec![0; T::Repr::PREFERRED_ARRAY_LEN];
        self.__priv_repr.to_u64_slice(&mut vec);
        vec
    }

    /// Copies the elements of this set into a `&mut [u64]`.
    ///
    /// If the underlying bitset will not fit in the provided slice, this method will panic.
    pub fn copy_into_slice(&self, data: &mut [u64]) {
        self.try_copy_into_slice(data)
            .expect("Bitset will not fit into slice.")
    }

    /// Copies the elements of this set into a `&mut [u64]`.
    ///
    /// If the underlying bitset will not fit in the provided slice, this method will return
    /// `None`. Otherwise, it will return `Some(())`.
    #[must_use]
    pub fn try_copy_into_slice(&self, data: &mut [u64]) -> Option<()> {
        self.__priv_repr.to_u64_slice_opt(data)
    }

    /// Copies the elements of this set into a `&mut [u64]`.
    ///
    /// If the underlying bitset will not fit in the provided slice, this method will truncate any
    /// bits that don't fit.
    pub fn copy_into_slice_truncated(&self, data: &mut [u64]) {
        self.__priv_repr.to_u64_slice(data)
    }

    /// Attempts to constructs a bitset from a `&[u64]`.
    ///
    /// If a bit that doesn't correspond to an enum variant is set, this method will panic.
    pub fn from_slice(v: &[u64]) -> Self {
        Self::try_from_slice(v).expect("Bitset contains invalid variants.")
    }

    /// Attempts to constructs a bitset from a `&[u64]`.
    ///
    /// If a bit that doesn't correspond to an enum variant is set, this method will return `None`.
    pub fn try_from_slice(bits: &[u64]) -> Option<Self> {
        let bits = T::Repr::from_u64_slice_opt(bits);
        let mask = T::ALL_BITS;
        bits.and_then(|bits| {
            if bits.and_not(mask).is_empty() {
                Some(EnumSet { __priv_repr: bits })
            } else {
                None
            }
        })
    }

    /// Constructs a bitset from a `&[u64]`, ignoring bits that do not correspond to a variant.
    pub fn from_slice_truncated(bits: &[u64]) -> Self {
        let bits = T::Repr::from_u64_slice(bits) & T::ALL_BITS;
        EnumSet { __priv_repr: bits }
    }

    /// Constructs a bitset from a `&[u64]`, without checking for invalid bits.
    ///
    /// # Safety
    ///
    /// All bits in the provided parameter `bits` that don't correspond to an enum variant
    /// of `T` must be set to `0`. Behavior is **undefined** if any of these bits are set
    /// to `1`.
    #[inline(always)]
    pub unsafe fn from_slice_unchecked(bits: &[u64]) -> Self {
        EnumSet { __priv_repr: T::Repr::from_u64_slice(bits) }
    }
}

impl<T: EnumSetType, const N: usize> From<[T; N]> for EnumSet<T> {
    fn from(value: [T; N]) -> Self {
        let mut new = EnumSet::new();
        for elem in value {
            new.insert(elem);
        }
        new
    }
}
//endregion

//region EnumSet iter
/// The iterator used by [`EnumSet`]s.
#[derive(Clone, Debug)]
pub struct EnumSetIter<T: EnumSetType> {
    iter: <T::Repr as EnumSetTypeRepr>::Iter,
}
impl<T: EnumSetType> EnumSetIter<T> {
    fn new(set: EnumSet<T>) -> EnumSetIter<T> {
        EnumSetIter { iter: set.__priv_repr.iter() }
    }
}

impl<T: EnumSetType> EnumSet<T> {
    /// Iterates the contents of the set in order from the least significant bit to the most
    /// significant bit.
    ///
    /// Note that iterator invalidation is impossible as the iterator contains a copy of this type,
    /// rather than holding a reference to it.
    pub fn iter(&self) -> EnumSetIter<T> {
        EnumSetIter::new(*self)
    }
}

impl<T: EnumSetType> Iterator for EnumSetIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter
            .next()
            .map(|x| unsafe { T::enum_from_u32_checked(x) })
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}

impl<T: EnumSetType> DoubleEndedIterator for EnumSetIter<T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.iter
            .next_back()
            .map(|x| unsafe { T::enum_from_u32_checked(x) })
    }
}

impl<T: EnumSetType> ExactSizeIterator for EnumSetIter<T> {}

impl<T: EnumSetType> Extend<T> for EnumSet<T> {
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        iter.into_iter().for_each(|v| {
            self.insert(v);
        });
    }
}

impl<'a, T: EnumSetType> Extend<&'a T> for EnumSet<T> {
    fn extend<I: IntoIterator<Item = &'a T>>(&mut self, iter: I) {
        iter.into_iter().for_each(|v| {
            self.insert(*v);
        });
    }
}

impl<T: EnumSetType> FromIterator<T> for EnumSet<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut set = EnumSet::default();
        set.extend(iter);
        set
    }
}

impl<T: EnumSetType> Extend<EnumSet<T>> for EnumSet<T> {
    fn extend<I: IntoIterator<Item = EnumSet<T>>>(&mut self, iter: I) {
        iter.into_iter().for_each(|v| {
            self.insert_all(v);
        });
    }
}

impl<'a, T: EnumSetType> Extend<&'a EnumSet<T>> for EnumSet<T> {
    fn extend<I: IntoIterator<Item = &'a EnumSet<T>>>(&mut self, iter: I) {
        iter.into_iter().for_each(|v| {
            self.insert_all(*v);
        });
    }
}

impl<T: EnumSetType> FromIterator<EnumSet<T>> for EnumSet<T> {
    fn from_iter<I: IntoIterator<Item = EnumSet<T>>>(iter: I) -> Self {
        let mut set = EnumSet::default();
        set.extend(iter);
        set
    }
}

impl<T: EnumSetType> IntoIterator for EnumSet<T> {
    type Item = T;
    type IntoIter = EnumSetIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}
impl<T: EnumSetType> Sum for EnumSet<T> {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.fold(EnumSet::empty(), |a, v| a | v)
    }
}
impl<'a, T: EnumSetType> Sum<&'a EnumSet<T>> for EnumSet<T> {
    fn sum<I: Iterator<Item = &'a Self>>(iter: I) -> Self {
        iter.fold(EnumSet::empty(), |a, v| a | *v)
    }
}
impl<T: EnumSetType> Sum<T> for EnumSet<T> {
    fn sum<I: Iterator<Item = T>>(iter: I) -> Self {
        iter.fold(EnumSet::empty(), |a, v| a | v)
    }
}
impl<'a, T: EnumSetType> Sum<&'a T> for EnumSet<T> {
    fn sum<I: Iterator<Item = &'a T>>(iter: I) -> Self {
        iter.fold(EnumSet::empty(), |a, v| a | *v)
    }
}
//endregion
