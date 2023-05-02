use crate::repr::EnumSetTypeRepr;
use crate::{EnumSet, EnumSetType};
use core::iter::Sum;

/// The iterator used by [`EnumSet`]s.
#[derive(Clone, Debug)]
pub struct EnumSetIter<T: EnumSetType> {
    set: EnumSet<T>,
}
impl<T: EnumSetType> EnumSetIter<T> {
    fn new(set: EnumSet<T>) -> EnumSetIter<T> {
        EnumSetIter { set }
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
        if self.set.is_empty() {
            None
        } else {
            let bit = self.set.__priv_repr.trailing_zeros();
            self.set.__priv_repr.remove_bit(bit);
            unsafe { Some(T::enum_from_u32(bit)) }
        }
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        let left = self.set.len();
        (left, Some(left))
    }
}

impl<T: EnumSetType> DoubleEndedIterator for EnumSetIter<T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.set.is_empty() {
            None
        } else {
            let bit = T::Repr::WIDTH - 1 - self.set.__priv_repr.leading_zeros();
            self.set.__priv_repr.remove_bit(bit);
            unsafe { Some(T::enum_from_u32(bit)) }
        }
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
