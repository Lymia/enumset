mod primitive;
mod array;

use core::fmt::Debug;
use core::hash::Hash;
use core::ops::*;

/// A trait marking valid underlying bitset storage types and providing the
/// operations `EnumSet` and related types use.
pub trait EnumSetTypeRepr :
    // Basic traits used to derive traits
    Copy +
    Ord +
    Eq +
    Debug +
    Hash +
    // Operations used by enumset
    BitAnd<Output = Self> +
    BitOr<Output = Self> +
    BitXor<Output = Self> +
    Not<Output = Self> +
{
    const WIDTH: u32;

    fn is_empty(&self) -> bool;
    fn empty() -> Self;

    fn add_bit(&mut self, bit: u32);
    fn remove_bit(&mut self, bit: u32);
    fn has_bit(&self, bit: u32) -> bool;

    fn count_ones(&self) -> u32;
    fn leading_zeros(&self) -> u32;
    fn trailing_zeros(&self) -> u32;

    fn and_not(&self, other: Self) -> Self;

    fn from_u8(v: u8) -> Self;
    fn from_u16(v: u16) -> Self;
    fn from_u32(v: u32) -> Self;
    fn from_u64(v: u64) -> Self;
    fn from_u128(v: u128) -> Self;
    fn from_usize(v: usize) -> Self;

    fn to_u8(&self) -> u8;
    fn to_u16(&self) -> u16;
    fn to_u32(&self) -> u32;
    fn to_u64(&self) -> u64;
    fn to_u128(&self) -> u128;
    fn to_usize(&self) -> usize;

    fn from_u8_opt(v: u8) -> Option<Self>;
    fn from_u16_opt(v: u16) -> Option<Self>;
    fn from_u32_opt(v: u32) -> Option<Self>;
    fn from_u64_opt(v: u64) -> Option<Self>;
    fn from_u128_opt(v: u128) -> Option<Self>;
    fn from_usize_opt(v: usize) -> Option<Self>;

    fn to_u8_opt(&self) -> Option<u8>;
    fn to_u16_opt(&self) -> Option<u16>;
    fn to_u32_opt(&self) -> Option<u32>;
    fn to_u64_opt(&self) -> Option<u64>;
    fn to_u128_opt(&self) -> Option<u128>;
    fn to_usize_opt(&self) -> Option<usize>;
}

pub use array::ArrayRepr;