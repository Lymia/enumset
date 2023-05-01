use crate::repr::EnumSetTypeRepr;
use core::ops::*;

/// An implementation of `EnumSetTypeRepr` based on an arbitrary size array.
///
/// `N` **must** be `2` or higher, or else logic errors will occur.
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Debug, Hash)]
pub struct ArrayRepr<const N: usize>(pub [u64; N]);
impl<const N: usize> ArrayRepr<N> {
    fn split_bit(bit: u32) -> (usize, u32) {
        (bit as usize / 64, bit % 64)
    }
}

impl<const N: usize> BitAnd for ArrayRepr<N> {
    type Output = Self;
    fn bitand(mut self, rhs: Self) -> Self::Output {
        for i in 0..N {
            self.0[i] &= rhs.0[i];
        }
        self
    }
}
impl<const N: usize> BitOr for ArrayRepr<N> {
    type Output = Self;
    fn bitor(mut self, rhs: Self) -> Self::Output {
        for i in 0..N {
            self.0[i] |= rhs.0[i];
        }
        self
    }
}
impl<const N: usize> BitXor for ArrayRepr<N> {
    type Output = Self;
    fn bitxor(mut self, rhs: Self) -> Self::Output {
        for i in 0..N {
            self.0[i] ^= rhs.0[i];
        }
        self
    }
}
impl<const N: usize> Not for ArrayRepr<N> {
    type Output = Self;
    fn not(mut self) -> Self::Output {
        for i in 0..N {
            self.0[i] = !self.0[i];
        }
        self
    }
}

impl<const N: usize> EnumSetTypeRepr for ArrayRepr<N> {
    const WIDTH: u32 = N as u32 * 64;

    fn is_empty(&self) -> bool {
        self.0.iter().all(|x| *x == 0)
    }
    fn empty() -> Self {
        ArrayRepr([0; N])
    }

    fn add_bit(&mut self, bit: u32) {
        let (idx, bit) = Self::split_bit(bit);
        self.0[idx].add_bit(bit);
    }
    fn remove_bit(&mut self, bit: u32) {
        let (idx, bit) = Self::split_bit(bit);
        self.0[idx].remove_bit(bit);
    }
    fn has_bit(&self, bit: u32) -> bool {
        let (idx, bit) = Self::split_bit(bit);
        self.0[idx].has_bit(bit)
    }

    fn count_ones(&self) -> u32 {
        self.0.iter().map(|x| x.count_ones()).sum()
    }
    fn leading_zeros(&self) -> u32 {
        let mut accum = 0;
        for i in (0..N).rev() {
            if self.0[i] != 0 {
                return accum + self.0[i].leading_zeros();
            }
            accum += 64;
        }
        Self::WIDTH
    }
    fn trailing_zeros(&self) -> u32 {
        let mut accum = 0;
        for i in 0..N {
            if self.0[i] != 0 {
                return accum + self.0[i].trailing_zeros();
            }
            accum += 64;
        }
        Self::WIDTH
    }

    fn and_not(&self, other: Self) -> Self {
        let mut new = Self([0; N]);
        for i in 0..N {
            new.0[i] = self.0[i] & !other.0[i];
        }
        new
    }

    fn from_u8(v: u8) -> Self {
        Self::from_u64(v as u64)
    }
    fn from_u16(v: u16) -> Self {
        Self::from_u64(v as u64)
    }
    fn from_u32(v: u32) -> Self {
        Self::from_u64(v as u64)
    }
    fn from_u64(v: u64) -> Self {
        let mut new = Self([0; N]);
        new.0[0] = v;
        new
    }
    fn from_u128(v: u128) -> Self {
        let mut new = Self([0; N]);
        new.0[0] = v as u64;
        new.0[1] = (v >> 64) as u64;
        new
    }
    fn from_usize(v: usize) -> Self {
        Self::from_u64(v as u64)
    }

    fn from_u8_opt(v: u8) -> Option<Self> {
        Some(Self::from_u8(v))
    }
    fn from_u16_opt(v: u16) -> Option<Self> {
        Some(Self::from_u16(v))
    }
    fn from_u32_opt(v: u32) -> Option<Self> {
        Some(Self::from_u32(v))
    }
    fn from_u64_opt(v: u64) -> Option<Self> {
        Some(Self::from_u64(v))
    }
    fn from_u128_opt(v: u128) -> Option<Self> {
        Some(Self::from_u128(v))
    }
    fn from_usize_opt(v: usize) -> Option<Self> {
        Some(Self::from_usize(v))
    }

    fn to_u8(&self) -> u8 {
        self.to_u64().to_u8()
    }
    fn to_u16(&self) -> u16 {
        self.to_u64().to_u16()
    }
    fn to_u32(&self) -> u32 {
        self.to_u64().to_u32()
    }
    fn to_u64(&self) -> u64 {
        self.0[0]
    }
    fn to_u128(&self) -> u128 {
        self.0[0] as u128 | ((self.0[0] as u128) << 64)
    }
    fn to_usize(&self) -> usize {
        self.to_u64().to_usize()
    }

    fn to_u8_opt(&self) -> Option<u8> {
        self.to_u64_opt().and_then(|x| x.to_u8_opt())
    }
    fn to_u16_opt(&self) -> Option<u16> {
        self.to_u64_opt().and_then(|x| x.to_u16_opt())
    }
    fn to_u32_opt(&self) -> Option<u32> {
        self.to_u64_opt().and_then(|x| x.to_u32_opt())
    }
    fn to_u64_opt(&self) -> Option<u64> {
        for i in 1..N {
            if self.0[i] != 0 {
                return None;
            }
        }
        Some(self.to_u64())
    }
    fn to_u128_opt(&self) -> Option<u128> {
        for i in 2..N {
            if self.0[i] != 0 {
                return None;
            }
        }
        Some(self.to_u128())
    }
    fn to_usize_opt(&self) -> Option<usize> {
        self.to_u64_opt().and_then(|x| x.to_usize_opt())
    }
}
