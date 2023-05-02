use crate::repr::EnumSetTypeRepr;
use crate::{EnumSet, EnumSetType, EnumSetTypeWithRepr};

impl<T: EnumSetType + EnumSetTypeWithRepr> EnumSet<T> {
    /// Returns a `T::Repr` representing the elements of this set.
    ///
    /// Unlike the other `as_*` methods, this method is zero-cost and guaranteed not to fail,
    /// panic or truncate any bits.
    ///
    /// In order to use this method, the definition of `T` must have the `#[enumset(repr = "…")]`
    /// annotation.
    #[inline(always)]
    pub fn as_repr(&self) -> <T as EnumSetTypeWithRepr>::Repr {
        self.__priv_repr
    }

    /// Constructs a bitset from a `T::Repr` without checking for invalid bits.
    ///
    /// Unlike the other `from_*` methods, this method is zero-cost and guaranteed not to fail,
    /// panic or truncate any bits, provided the conditions under “Safety” are upheld.
    ///
    /// In order to use this method, the definition of `T` must have the `#[enumset(repr = "…")]`
    /// annotation.
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
    /// In order to use this method, the definition of `T` must have the `#[enumset(repr = "…")]`
    /// annotation.
    #[inline(always)]
    pub fn from_repr(bits: <T as EnumSetTypeWithRepr>::Repr) -> Self {
        Self::try_from_repr(bits).expect("Bitset contains invalid variants.")
    }

    /// Attempts to constructs a bitset from a `T::Repr`.
    ///
    /// If a bit that doesn't correspond to an enum variant is set, this
    /// method will return `None`.
    ///
    /// In order to use this method, the definition of `T` must have the `#[enumset(repr = "…")]`
    /// annotation.
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
    /// In order to use this method, the definition of `T` must have the `#[enumset(repr = "…")]`
    /// annotation.
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
    /// If the underlying bitset will not fit in a `[u8; O]`, this method will panic.
    pub fn as_array<const O: usize>(&self) -> [u64; O] {
        self.try_as_array()
            .expect("Bitset will not fit into this type.")
    }

    /// Returns an `[u64; O]` representing the elements of this set.
    ///
    /// If the underlying bitset will not fit in a `[u8; O]`, this method will instead return
    /// `None`.
    pub fn try_as_array<const O: usize>(&self) -> Option<[u64; O]> {
        self.__priv_repr.to_u64_array_opt()
    }

    /// Returns an `[u64; O]` representing the elements of this set.
    ///
    /// If the underlying bitset will not fit in a `[u8; O]`, this method will truncate any bits
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

    // TODO: Conversions to vec/read to slice

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
    pub fn from_slice_truncated<const O: usize>(bits: &[u64]) -> Self {
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
    pub unsafe fn from_slice_unchecked<const O: usize>(bits: &[u64]) -> Self {
        EnumSet { __priv_repr: T::Repr::from_u64_slice(bits) }
    }
}
