use crate::repr::EnumSetTypeRepr;

#[cfg(feature = "serde")]
use {crate::EnumSet, serde2 as serde};

/// The trait used to define enum types that may be used with [`EnumSet`].
///
/// This trait must be impelmented using `#[derive(EnumSetType)]`, is not public API, and its
/// internal structure may change at any time with no warning.
///
/// For full documentation on the procedural derive and its options, see
/// [`#[derive(EnumSetType)]`](derive@crate::EnumSetType).
///
/// [`EnumSet`]: crate::set::EnumSet
pub unsafe trait EnumSetType: Copy + Eq + EnumSetTypePrivate {}

/// An [`EnumSetType`] for which [`EnumSet`]s have a guaranteed in-memory representation.
///
/// An implementation of this trait is generated by using
/// [`#[derive(EnumSetType)]`](derive@crate::EnumSetType) with the annotation
/// `#[enumset(repr = "…")]`, where `…` is `u8`, `u16`, `u32`, `u64` or `u128`.
///
/// For any type `T` that implements this trait, the in-memory representation of `EnumSet<T>`
/// is guaranteed to be `Repr`. This guarantee is useful for FFI. See [the `EnumSet` documentation
/// under “FFI, Safety and `repr`”][crate::set::EnumSet#ffi-safety-and-repr] for an example.
///
/// [`EnumSet`]: crate::set::EnumSet
pub unsafe trait EnumSetTypeWithRepr:
    EnumSetType + EnumSetTypePrivate<Repr = <Self as EnumSetTypeWithRepr>::Repr>
{
    /// The guaranteed representation.
    type Repr: EnumSetTypeRepr;
}

/// The actual members of EnumSetType. Put here to avoid polluting global namespaces.
pub unsafe trait EnumSetTypePrivate {
    /// The underlying type used to store the bitset.
    type Repr: EnumSetTypeRepr;
    /// A mask of bits that are valid in the bitset.
    const ALL_BITS: Self::Repr;

    /// Converts an enum of this type into its bit position.
    fn enum_into_u32(self) -> u32;
    /// Converts a bit position into an enum value.
    unsafe fn enum_from_u32(val: u32) -> Self;

    /// Serializes the `EnumSet`.
    ///
    /// This and `deserialize` are part of the `EnumSetType` trait so the procedural derive
    /// can control how `EnumSet` is serialized.
    #[cfg(feature = "serde")]
    fn serialize<S: serde::Serializer>(set: EnumSet<Self>, ser: S) -> Result<S::Ok, S::Error>
    where Self: EnumSetType;
    /// Deserializes the `EnumSet`.
    #[cfg(feature = "serde")]
    fn deserialize<'de, D: serde::Deserializer<'de>>(de: D) -> Result<EnumSet<Self>, D::Error>
    where Self: EnumSetType;
}
