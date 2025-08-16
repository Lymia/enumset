use crate::traits::EnumSetTypePrivate;
use crate::EnumSetType;
use core::cmp::Ordering;
use core::fmt::{Debug, Formatter};
use core::hash::{Hash, Hasher};
use core::iter::Enumerate;
use core::marker::PhantomData;
use core::{array, slice};

pub trait EnumRecordUnderlying {
    const N: usize;
    type Value;
    fn create(func: impl FnMut(usize) -> Self::Value) -> Self;

    fn get(&self, idx: usize) -> &Self::Value;
    fn get_mut(&mut self, idx: usize) -> &mut Self::Value;

    fn iter(&self) -> slice::Iter<'_, Self::Value>;
    fn iter_mut(&mut self) -> slice::IterMut<'_, Self::Value>;

    type IntoIter: Iterator<Item = Self::Value>;
    fn into_iter(self) -> Self::IntoIter;
}

impl<const N: usize, V> EnumRecordUnderlying for [V; N] {
    const N: usize = N;
    type Value = V;
    fn create(func: impl FnMut(usize) -> Self::Value) -> Self {
        array::from_fn(func)
    }

    fn get(&self, idx: usize) -> &Self::Value {
        &self[idx]
    }
    fn get_mut(&mut self, idx: usize) -> &mut Self::Value {
        &mut self[idx]
    }

    fn iter(&self) -> slice::Iter<'_, Self::Value> {
        self.as_slice().iter()
    }
    fn iter_mut(&mut self) -> slice::IterMut<'_, Self::Value> {
        self.as_mut_slice().iter_mut()
    }

    type IntoIter = array::IntoIter<V, N>;
    fn into_iter(self) -> Self::IntoIter {
        IntoIterator::into_iter(self)
    }
}

pub struct EnumRecord<K: EnumSetType, V> {
    underlying: K::RecordArray<V>,
}
impl<K: EnumSetType, V> EnumRecord<K, V> {
    pub fn from_fn(mut func: impl FnMut(K) -> V) -> Self {
        EnumRecord {
            underlying: K::RecordArray::<V>::create(|x| {
                func(unsafe { K::compact_enum_from_u32_checked(x as u32) })
            }),
        }
    }

    pub fn repeat(v: V) -> Self
    where V: Clone {
        EnumRecord { underlying: K::RecordArray::<V>::create(|_| v.clone()) }
    }
}

//region Basic traits
impl<K: EnumSetType, V: Clone> Clone for EnumRecord<K, V>
where K::RecordArray<V>: Clone
{
    fn clone(&self) -> Self {
        EnumRecord { underlying: self.underlying.clone() }
    }
}
impl<K: EnumSetType, V: Copy + Clone> Copy for EnumRecord<K, V> where K::RecordArray<V>: Copy + Clone
{}
impl<K: EnumSetType, V: PartialEq> PartialEq for EnumRecord<K, V>
where K::RecordArray<V>: PartialEq
{
    fn eq(&self, other: &Self) -> bool {
        self.underlying.eq(&other.underlying)
    }
}
impl<K: EnumSetType, V: PartialEq + Eq> Eq for EnumRecord<K, V> where K::RecordArray<V>: PartialEq + Eq
{}
impl<K: EnumSetType, V: PartialOrd> PartialOrd for EnumRecord<K, V>
where K::RecordArray<V>: PartialOrd
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.underlying.partial_cmp(&other.underlying)
    }
}
impl<K: EnumSetType, V: PartialOrd + Ord> Ord for EnumRecord<K, V>
where K::RecordArray<V>: PartialOrd + Ord
{
    fn cmp(&self, other: &Self) -> Ordering {
        self.underlying.cmp(&other.underlying)
    }
}
impl<K: EnumSetType, V: Hash> Hash for EnumRecord<K, V>
where K::RecordArray<V>: Hash
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.underlying.hash(state)
    }
}
impl<K: EnumSetType, V: Default> Default for EnumRecord<K, V>
where K::RecordArray<V>: Default
{
    fn default() -> Self {
        EnumRecord { underlying: K::RecordArray::<V>::default() }
    }
}
impl<K: EnumSetType, V: Debug> Debug for EnumRecord<K, V>
where K::RecordArray<V>: Debug
{
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        let mut map = f.debug_map();
        map.finish()?;
        Ok(())
    }
}
//endregion

//region EnumRecord iter
macro_rules! record_impls {
    (
        $iter_ty:ident ($($borrow:tt)*) ($lt:lifetime $($item_lt:lifetime)*) ($($bound:tt)*)
        ($($mut_flag:tt)*) double_ended $iter_fn:ident, $doc:literal
    ) => {
        impl<$($bound)* K: EnumSetType, V> DoubleEndedIterator for $iter_ty<$($bound)* K, V> {
            fn next_back(&mut self) -> Option<Self::Item> {
                unsafe {
                    self.iter.next_back().map(|(k, v)|
                        (K::compact_enum_from_u32_checked(k as u32), v)
                    )
                }
            }
        }
        record_impls!(
            $iter_ty ($($borrow)*) ($lt $($item_lt)*) ($($bound)*) ($($mut_flag)*) $iter_fn, $doc
        );
    };
    (
        $iter_ty:ident ($($borrow:tt)*) ($lt:lifetime $($item_lt:lifetime)*) ($($bound:tt)*)
        ($($mut_flag:tt)*) $iter_fn:ident, $doc:literal
    ) => {
        impl<K: EnumSetType, V> EnumRecord<K, V> {
            #[doc = $doc]
            pub fn $iter_fn<$($bound)*>(
                $($borrow)* $($item_lt)* $($mut_flag)* self,
            ) -> $iter_ty<$($bound)* K, V> {
                $iter_ty {
                    iter: self.underlying.$iter_fn().enumerate(),
                    _phantom: PhantomData,
                }
            }
        }
        impl<$($bound)* K: EnumSetType, V> Iterator for $iter_ty<$($bound)* K, V> {
            type Item = (K, $($borrow)* $($item_lt)* $($mut_flag)* V);

            fn next(&mut self) -> Option<Self::Item> {
                unsafe {
                    self.iter.next().map(|(k, v)|
                        (K::compact_enum_from_u32_checked(k as u32), v)
                    )
                }
            }
            fn size_hint(&self) -> (usize, Option<usize>) {
                self.iter.size_hint()
            }
        }
        impl<$($bound)* K: EnumSetType, V> ExactSizeIterator for $iter_ty<$($bound)* K, V> {}
    };
}

/// The iterator used by [`EnumRecords`]s.
pub struct EnumRecordIter<'a, K: EnumSetType, V> {
    iter: Enumerate<slice::Iter<'a, V>>,
    _phantom: PhantomData<K>,
}
record_impls!(
    EnumRecordIter (&) ('a 'a) ('a,) () double_ended iter,
    "Iterates the contents of the record in an unspecified order."
);

/// The mutable iterator used by [`EnumRecords`]s.
pub struct EnumRecordIterMut<'a, K: EnumSetType, V> {
    iter: Enumerate<slice::IterMut<'a, V>>,
    _phantom: PhantomData<K>,
}
record_impls!(
    EnumRecordIterMut (&) ('a 'a) ('a,) (mut) double_ended iter_mut,
    "Mutably iterates the contents of the record in an unspecified order."
);

/// The mutable iterator used by [`EnumRecords`]s.
pub struct EnumRecordIntoIter<K: EnumSetType, V> {
    iter: Enumerate<<K::RecordArray<V> as EnumRecordUnderlying>::IntoIter>,
    _phantom: PhantomData<K>,
}
record_impls!(
    EnumRecordIntoIter () ('a) () () into_iter,
    "Converts the contents of the record into an iterator."
);
//endregion
