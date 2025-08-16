macro_rules! set_common_impls {
    ($T:ty, $T_Repr:ty) => {
        /// Returns a set containing a single element.
        #[inline(always)]
        pub fn only(t: T) -> Self {
            let mut set = Self::new();
            set.insert(t);
            set
        }

        /// Returns the bit a given enum variant is stored in.
        ///
        /// If this returns `n`, it means the bit is stored in the `n`th least significant bit of the
        /// underlying integer.
        pub fn bit_index(t: T) -> u32 {
            T::enum_into_u32(t)
        }

        /// Returns whether a given bit is valid for this set.
        pub fn is_bit_valid(bit: u32) -> bool {
            T::ALL_BITS.has_bit(bit)
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
            self.__priv_repr = <$T_Repr>::EMPTY;
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
            Self { __priv_repr: self.__priv_repr | other.__priv_repr }
        }
        /// Returns a set containing every element present in both sets.
        #[inline(always)]
        pub fn intersection(&self, other: Self) -> Self {
            Self { __priv_repr: self.__priv_repr & other.__priv_repr }
        }
        /// Returns a set containing element present in `self` but not in `other`.
        #[inline(always)]
        pub fn difference(&self, other: Self) -> Self {
            Self { __priv_repr: self.__priv_repr.and_not(other.__priv_repr) }
        }
        /// Returns a set containing every element present in either `self` or `other`, but is not
        /// present in both.
        #[inline(always)]
        pub fn symmetrical_difference(&self, other: Self) -> Self {
            Self { __priv_repr: self.__priv_repr ^ other.__priv_repr }
        }
        /// Returns a set containing all enum variants not in this set.
        #[inline(always)]
        pub fn complement(&self) -> Self {
            Self { __priv_repr: !self.__priv_repr & <$T>::ALL_BITS }
        }

        /// Checks whether this set contains a value.
        #[inline(always)]
        pub fn contains(&self, value: $T) -> bool {
            self.__priv_repr.has_bit(value.enum_into_u32())
        }

        /// Adds a value to this set.
        ///
        /// If the set did not have this value present, `true` is returned.
        ///
        /// If the set did have this value present, `false` is returned.
        #[inline(always)]
        pub fn insert(&mut self, value: $T) -> bool {
            let contains = !self.contains(value);
            self.__priv_repr.add_bit(value.enum_into_u32());
            contains
        }
        /// Removes a value from this set. Returns whether the value was present in the set.
        #[inline(always)]
        pub fn remove(&mut self, value: $T) -> bool {
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
    };
}
