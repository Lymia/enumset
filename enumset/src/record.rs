use crate::EnumSetType;
use core::cmp::Ordering;
use core::fmt::{Debug, Formatter};
use core::hash::{Hash, Hasher};
use core::iter::Enumerate;
use core::marker::PhantomData;
use core::ops::{Index, IndexMut};
use core::{array, slice};

#[allow(missing_docs)]
pub trait EnumRecordUnderlying {
    const N: usize;
    type Value;

    fn create(func: impl FnMut(usize) -> Self::Value) -> Self;
    unsafe fn as_ptr(ptr: *mut Self) -> *mut Self::Value;

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
    unsafe fn as_ptr(ptr: *mut Self) -> *mut Self::Value {
        ptr as *mut Self::Value
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

/// A collection that maps enum keys to values.
///
/// Unlike a typical map, this collection is *injective*, and maps every variant of an enum to some
/// value. As such, it behaves more like an array than a typical map.
///
/// This type is backed by a simple array of values. As such, it can often be too large to use
/// effectively on the stack. Methods are provided to efficiently construct boxed `EnumRecord`s.
///
/// # FFI Safety
///
/// `EnumRecord` is not FFI safe.
#[repr(transparent)]
pub struct EnumRecord<K: EnumSetType, V> {
    underlying: K::RecordArray<V>,
}
impl<K: EnumSetType, V> EnumRecord<K, V> {
    #[cfg(feature = "alloc")]
    #[cfg_attr(docsrs, doc(cfg(feature = "alloc")))]
    unsafe fn boxed_init(mut func: impl FnMut(usize) -> V) -> alloc::boxed::Box<Self> {
        let alloc = alloc::alloc::alloc(core::alloc::Layout::new::<K::RecordArray<V>>());
        let alloc = alloc as *mut K::RecordArray<V>;
        let direct_ptr = K::RecordArray::<V>::as_ptr(alloc);
        for i in 0..K::VARIANT_COUNT {
            let value = func(i as usize);
            direct_ptr.offset(i as isize).write(value);
        }
        alloc::boxed::Box::from_raw(direct_ptr as *mut Self)
    }

    /// Constructs a new record from a function.
    ///
    /// Each value in the record will be the result of calling the function on the corresponding
    /// key.
    pub fn from_fn(mut func: impl FnMut(K) -> V) -> Self {
        EnumRecord {
            underlying: K::RecordArray::<V>::create(|x| {
                func(unsafe { K::compact_enum_from_u32_checked(x as u32) })
            }),
        }
    }

    /// Constructs a new boxed record from a function.
    ///
    /// Each value in the record will be the result of calling the function on the corresponding
    /// key.
    #[cfg(feature = "alloc")]
    #[cfg_attr(docsrs, doc(cfg(feature = "alloc")))]
    pub fn from_fn_boxed(mut func: impl FnMut(K) -> V) -> alloc::boxed::Box<Self> {
        unsafe { Self::boxed_init(|x| func(K::compact_enum_from_u32_checked(x as u32))) }
    }

    /// Constructs a new record from a single value.
    ///
    /// Each value in the record will be a clone of the given value.
    pub fn repeat(v: V) -> Self
    where V: Clone {
        EnumRecord { underlying: K::RecordArray::<V>::create(|_| v.clone()) }
    }

    /// Constructs a new boxed record from a single value.
    ///
    /// Each value in the record will be a clone of the given value.
    #[cfg(feature = "alloc")]
    #[cfg_attr(docsrs, doc(cfg(feature = "alloc")))]
    pub fn repeat_boxed(v: V) -> alloc::boxed::Box<Self>
    where V: Clone {
        unsafe { Self::boxed_init(|_| v.clone()) }
    }

    /// Constructs a new record based on the values of this one.
    ///
    /// Each value in the new record will be the result of calling the function on the
    /// corresponding key and the existing value in this record.
    pub fn map<V2>(&self, mut func: impl FnMut(K, &V) -> V2) -> EnumRecord<K, V2> {
        EnumRecord {
            underlying: K::RecordArray::<V2>::create(|x| {
                func(unsafe { K::compact_enum_from_u32_checked(x as u32) }, self.underlying.get(x))
            }),
        }
    }

    /// Constructs a new boxed record based on the values of this one.
    ///
    /// Each value in the new record will be the result of calling the function on the
    /// corresponding key and the existing value in this record.
    #[cfg(feature = "alloc")]
    #[cfg_attr(docsrs, doc(cfg(feature = "alloc")))]
    pub fn map_boxed<V2>(
        &self,
        mut func: impl FnMut(K, &V) -> V2,
    ) -> alloc::boxed::Box<EnumRecord<K, V2>> {
        unsafe {
            EnumRecord::boxed_init(|x| {
                func(K::compact_enum_from_u32_checked(x as u32), self.underlying.get(x))
            })
        }
    }
}
impl<K: EnumSetType, V> Index<K> for EnumRecord<K, V> {
    type Output = V;
    fn index(&self, index: K) -> &Self::Output {
        self.underlying
            .get(K::compact_enum_into_u32(index) as usize)
    }
}
impl<K: EnumSetType, V> IndexMut<K> for EnumRecord<K, V> {
    fn index_mut(&mut self, index: K) -> &mut <Self as Index<K>>::Output {
        self.underlying
            .get_mut(K::compact_enum_into_u32(index) as usize)
    }
}

//region Basic traits
impl<K: EnumSetType, V: Clone> Clone for EnumRecord<K, V>
where K::RecordArray<V>: Clone
{
    fn clone(&self) -> Self {
        EnumRecord { underlying: self.underlying.clone() }
    }

    fn clone_from(&mut self, source: &Self)
    where Self: {
        self.underlying.clone_from(&source.underlying)
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
impl<K: EnumSetType + Debug, V: Debug> Debug for EnumRecord<K, V>
where K::RecordArray<V>: Debug
{
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        let mut map = f.debug_map();
        for (k, v) in self {
            map.entry(&k, v);
        }
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
        impl <$($bound)* K: EnumSetType, V> IntoIterator for
            $($borrow)* $($item_lt)* $($mut_flag)* EnumRecord<K, V>
        {
            type Item = (K, $($borrow)* $($item_lt)* $($mut_flag)* V);
            type IntoIter = $iter_ty<$($bound)* K, V>;
            fn into_iter(self) -> Self::IntoIter {
                self.$iter_fn()
            }
        }
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
