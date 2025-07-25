//! This module handles generating the actual code to allow an enum type to be used as a bitset.

use crate::plan::{EnumSetInfo, InternalRepr, SerdeRepr};
use proc_macro2::{Literal, Span, TokenStream as SynTokenStream};
use quote::*;
use syn::*;

/// Generates the actual `EnumSetType` impl.
pub fn generate_code(info: EnumSetInfo) -> SynTokenStream {
    let name = &info.name;

    let enumset = match &info.crate_name {
        Some(crate_name) => quote!(::#crate_name),
        None => {
            #[cfg(feature = "proc-macro-crate")]
            {
                use proc_macro_crate::FoundCrate;

                let crate_name = proc_macro_crate::crate_name("enumset");
                match crate_name {
                    Ok(FoundCrate::Name(name)) => {
                        let ident = Ident::new(&name, Span::call_site());
                        quote!(::#ident)
                    }
                    _ => quote!(::enumset),
                }
            }

            #[cfg(not(feature = "proc-macro-crate"))]
            {
                quote!(::enumset)
            }
        }
    };
    let typed_enumset = quote!(#enumset::EnumSet<#name>);
    let core = quote!(#enumset::__internal::core_export);
    let internal = quote!(#enumset::__internal);
    let serde = quote!(#enumset::__internal::serde);

    let repr = match info.internal_repr() {
        InternalRepr::U8 => quote! { u8 },
        InternalRepr::U16 => quote! { u16 },
        InternalRepr::U32 => quote! { u32 },
        InternalRepr::U64 => quote! { u64 },
        InternalRepr::U128 => quote! { u128 },
        InternalRepr::Array(size) => quote! { #internal::ArrayRepr<{ #size }> },
    };
    let variant_map = info.variant_map();
    let all_variants = match info.internal_repr() {
        InternalRepr::U8 | InternalRepr::U16 | InternalRepr::U32 | InternalRepr::U64 => {
            let lit = Literal::u64_unsuffixed(variant_map[0]);
            quote! { #lit }
        }
        InternalRepr::U128 => {
            let lit = Literal::u128_unsuffixed(
                variant_map[0] as u128 | variant_map.get(1).map_or(0, |x| (*x as u128) << 64),
            );
            quote! { #lit }
        }
        InternalRepr::Array(size) => {
            let mut new = Vec::new();
            for i in 0..size {
                new.push(Literal::u64_unsuffixed(*variant_map.get(i).unwrap_or(&0)));
            }
            quote! { #internal::ArrayRepr::<{ #size }>([#(#new,)*]) }
        }
    };

    let ops = if info.no_ops {
        quote! {}
    } else {
        quote! {
            #[automatically_derived]
            impl<O: Into<#typed_enumset>> #core::ops::Sub<O> for #name {
                type Output = #typed_enumset;
                fn sub(self, other: O) -> Self::Output {
                    #enumset::EnumSet::only(self) - other.into()
                }
            }
            #[automatically_derived]
            impl<O: Into<#typed_enumset>> #core::ops::BitAnd<O> for #name {
                type Output = #typed_enumset;
                fn bitand(self, other: O) -> Self::Output {
                    #enumset::EnumSet::only(self) & other.into()
                }
            }
            #[automatically_derived]
            impl<O: Into<#typed_enumset>> #core::ops::BitOr<O> for #name {
                type Output = #typed_enumset;
                fn bitor(self, other: O) -> Self::Output {
                    #enumset::EnumSet::only(self) | other.into()
                }
            }
            #[automatically_derived]
            impl<O: Into<#typed_enumset>> #core::ops::BitXor<O> for #name {
                type Output = #typed_enumset;
                fn bitxor(self, other: O) -> Self::Output {
                    #enumset::EnumSet::only(self) ^ other.into()
                }
            }
            #[automatically_derived]
            impl #core::ops::Not for #name {
                type Output = #typed_enumset;
                fn not(self) -> Self::Output {
                    !#enumset::EnumSet::only(self)
                }
            }
            #[automatically_derived]
            impl #core::cmp::PartialEq<#typed_enumset> for #name {
                fn eq(&self, other: &#typed_enumset) -> bool {
                    #enumset::EnumSet::only(*self) == *other
                }
            }
        }
    };

    let serde_repr = info.serde_repr();
    let serde_ops = match serde_repr {
        SerdeRepr::U8 | SerdeRepr::U16 | SerdeRepr::U32 | SerdeRepr::U64 | SerdeRepr::U128 => {
            let (serialize_repr, from_fn, to_fn) = match serde_repr {
                SerdeRepr::U8 => (quote! { u8 }, quote! { from_u8 }, quote! { to_u8 }),
                SerdeRepr::U16 => (quote! { u16 }, quote! { from_u16 }, quote! { to_u16 }),
                SerdeRepr::U32 => (quote! { u32 }, quote! { from_u32 }, quote! { to_u32 }),
                SerdeRepr::U64 => (quote! { u64 }, quote! { from_u64 }, quote! { to_u64 }),
                SerdeRepr::U128 => (quote! { u128 }, quote! { from_u128 }, quote! { to_u128 }),
                _ => unreachable!(),
            };
            let check_unknown = if info.serialize_deny_unknown {
                quote! {
                    if value & !#all_variants != 0 {
                        use #serde::de::Error;
                        return #core::prelude::v1::Err(
                            D::Error::custom("enumset contains unknown bits")
                        )
                    }
                }
            } else {
                quote! {}
            };
            quote! {
                fn serialize<S: #serde::Serializer>(
                    set: #enumset::EnumSet<#name>, ser: S,
                ) -> #core::result::Result<S::Ok, S::Error> {
                    let value =
                        <#repr as #enumset::__internal::EnumSetTypeRepr>::#to_fn(&set.__priv_repr);
                    #serde::Serialize::serialize(&value, ser)
                }
                fn deserialize<'de, D: #serde::Deserializer<'de>>(
                    de: D,
                ) -> #core::result::Result<#enumset::EnumSet<#name>, D::Error> {
                    let value = <#serialize_repr as #serde::Deserialize>::deserialize(de)?;
                    #check_unknown
                    let value = <#repr as #enumset::__internal::EnumSetTypeRepr>::#from_fn(value);
                    #core::prelude::v1::Ok(#enumset::EnumSet {
                        __priv_repr: value & #all_variants,
                    })
                }
            }
        }
        SerdeRepr::List => {
            let expecting_str = format!("a list of {name}");
            quote! {
                fn serialize<S: #serde::Serializer>(
                    set: #enumset::EnumSet<#name>, ser: S,
                ) -> #core::result::Result<S::Ok, S::Error> {
                    use #serde::ser::SerializeSeq;
                    let mut seq = ser.serialize_seq(#core::prelude::v1::Some(set.len()))?;
                    for bit in set {
                        seq.serialize_element(&bit)?;
                    }
                    seq.end()
                }
                fn deserialize<'de, D: #serde::Deserializer<'de>>(
                    de: D,
                ) -> #core::result::Result<#enumset::EnumSet<#name>, D::Error> {
                    struct Visitor;
                    impl <'de> #serde::de::Visitor<'de> for Visitor {
                        type Value = #enumset::EnumSet<#name>;
                        fn expecting(
                            &self, formatter: &mut #core::fmt::Formatter,
                        ) -> #core::fmt::Result {
                            write!(formatter, #expecting_str)
                        }
                        fn visit_seq<A>(
                            mut self, mut seq: A,
                        ) -> #core::result::Result<Self::Value, A::Error> where
                            A: #serde::de::SeqAccess<'de>
                        {
                            let mut accum = #enumset::EnumSet::<#name>::new();
                            while let #core::prelude::v1::Some(val) = seq.next_element::<#name>()? {
                                accum |= val;
                            }
                            #core::prelude::v1::Ok(accum)
                        }
                    }
                    de.deserialize_seq(Visitor)
                }
            }
        }
        SerdeRepr::Map => {
            let expecting_str = format!("a map from {name} to bool");
            quote! {
                fn serialize<S: #serde::Serializer>(
                    set: #enumset::EnumSet<#name>, ser: S,
                ) -> #core::result::Result<S::Ok, S::Error> {
                    use #serde::ser::SerializeMap;
                    let mut map = ser.serialize_map(#core::prelude::v1::Some(set.len()))?;
                    for bit in set {
                        map.serialize_entry(&bit, &true)?;
                    }
                    map.end()
                }
                fn deserialize<'de, D: #serde::Deserializer<'de>>(
                    de: D,
                ) -> #core::result::Result<#enumset::EnumSet<#name>, D::Error> {
                    struct Visitor;
                    impl <'de> #serde::de::Visitor<'de> for Visitor {
                        type Value = #enumset::EnumSet<#name>;
                        fn expecting(
                            &self, formatter: &mut #core::fmt::Formatter,
                        ) -> #core::fmt::Result {
                            write!(formatter, #expecting_str)
                        }
                        fn visit_map<A>(
                            mut self, mut map: A,
                        ) -> #core::result::Result<Self::Value, A::Error> where
                            A: #serde::de::MapAccess<'de>
                        {
                            let mut accum = #enumset::EnumSet::<#name>::new();
                            while let #core::prelude::v1::Some((val, is_present)) =
                                map.next_entry::<#name, bool>()?
                            {
                                if is_present {
                                    accum |= val;
                                }
                            }
                            #core::prelude::v1::Ok(accum)
                        }
                    }
                    de.deserialize_map(Visitor)
                }
            }
        }
        SerdeRepr::Array => {
            let preferred_size = quote! {
                <<#name as #internal::EnumSetTypePrivate>::Repr as #internal::EnumSetTypeRepr>
                    ::PREFERRED_ARRAY_LEN
            };
            let (check_extra, convert_array) = if info.serialize_deny_unknown {
                (
                    quote! {
                        if _val != 0 {
                            return #core::prelude::v1::Err(
                                D::Error::custom("enumset contains unknown bits")
                            )
                        }
                    },
                    quote! {
                        match #enumset::EnumSet::<#name>::try_from_array(accum) {
                            Some(x) => x,
                            None => #core::prelude::v1::Err(
                                D::Error::custom("enumset contains unknown bits")
                            ),
                        }
                    },
                )
            } else {
                (quote! {}, quote! {
                    #core::prelude::v1::Ok(#enumset::EnumSet::<#name>::from_array(accum))
                })
            };
            quote! {
                fn serialize<S: #serde::Serializer>(
                    set: #enumset::EnumSet<#name>, ser: S,
                ) -> #core::result::Result<S::Ok, S::Error> {
                    // read the enum as an array
                    let array = set.as_array::<{ #preferred_size }>();

                    // find the last non-zero value in the array
                    let mut end = array.len();
                    for i in (0..array.len()).rev() {
                        if array[i] != 0 {
                            break;
                        }
                        end = i + 1;
                    }

                    // serialize the array
                    #serde::Serialize::serialize(&array[..end], ser)
                }
                fn deserialize<'de, D: #serde::Deserializer<'de>>(
                    de: D,
                ) -> #core::result::Result<#enumset::EnumSet<#name>, D::Error> {
                    struct Visitor;
                    impl <'de> #serde::de::Visitor<'de> for Visitor {
                        type Value = #enumset::EnumSet<#name>;
                        fn expecting(
                            &self, formatter: &mut #core::fmt::Formatter,
                        ) -> #core::fmt::Result {
                            write!(formatter, "a list of u64")
                        }
                        fn visit_seq<A>(
                            mut self, mut seq: A,
                        ) -> #core::result::Result<Self::Value, A::Error> where
                            A: #serde::de::SeqAccess<'de>
                        {
                            let mut accum = [0; #preferred_size];

                            let mut i = 0;
                            while let #core::prelude::v1::Some(val) = seq.next_element::<u64>()? {
                                accum[i] = val;
                                i += 1;

                                if i == accum.len() {
                                    break;
                                }
                            }
                            while let #core::prelude::v1::Some(_val) = seq.next_element::<u64>()? {
                                #check_extra
                            }

                            #convert_array
                        }
                    }
                    de.deserialize_seq(Visitor)
                }
            }
        }
    };

    let is_uninhabited = info.variants.is_empty();
    let is_zst = info.variants.len() == 1;
    let into_impl = if is_uninhabited {
        quote! {
            fn enum_into_u32(self) -> u32 {
                panic!(concat!(stringify!(#name), " is uninhabited."))
            }
            unsafe fn enum_from_u32(val: u32) -> Self {
                panic!(concat!(stringify!(#name), " is uninhabited."))
            }
        }
    } else if is_zst {
        let variant = &info.variants[0].name;
        quote! {
            fn enum_into_u32(self) -> u32 {
                self as u32
            }
            unsafe fn enum_from_u32(val: u32) -> Self {
                #name::#variant
            }
        }
    } else {
        let variant_name: Vec<_> = info.variants.iter().map(|x| &x.name).collect();
        let variant_value: Vec<_> = info.variants.iter().map(|x| x.variant_repr).collect();

        let const_field: Vec<_> = ["IS_U8", "IS_U16", "IS_U32", "IS_U64", "IS_U128"]
            .iter()
            .map(|x| Ident::new(x, Span::call_site()))
            .collect();
        let int_type: Vec<_> = ["u8", "u16", "u32", "u64", "u128"]
            .iter()
            .map(|x| Ident::new(x, Span::call_site()))
            .collect();

        quote! {
            fn enum_into_u32(self) -> u32 {
                self as u32
            }
            unsafe fn enum_from_u32(val: u32) -> Self {
                // We put these in const fields so the branches they guard aren't generated even
                // on -O0
                #(const #const_field: bool =
                    #core::mem::size_of::<#name>() == #core::mem::size_of::<#int_type>();)*
                match val {
                    // Every valid variant value has an explicit branch. If they get optimized out,
                    // great. If the representation has changed somehow, and they don't, oh well,
                    // there's still no UB.
                    #(#variant_value => #name::#variant_name,)*
                    // Helps hint to the LLVM that this is a transmute. Note that this branch is
                    // still unreachable.
                    #(x if #const_field => {
                        let x = x as #int_type;
                        *(&x as *const _ as *const #name)
                    })*
                    // Default case. Sometimes causes LLVM to generate a table instead of a simple
                    // transmute, but, oh well.
                    _ => #core::hint::unreachable_unchecked(),
                }
            }
        }
    };

    let eq_impl = if is_uninhabited {
        quote!(panic!(concat!(stringify!(#name), " is uninhabited.")))
    } else {
        quote!((*self as u32) == (*other as u32))
    };

    let super_impls = if info.no_super_impls {
        quote! {}
    } else {
        quote! {
            #[automatically_derived]
            impl #core::cmp::PartialEq for #name {
                fn eq(&self, other: &Self) -> bool {
                    #eq_impl
                }
            }
            #[automatically_derived]
            impl #core::cmp::Eq for #name { }
            #[automatically_derived]
            #[allow(clippy::expl_impl_clone_on_copy)]
            impl #core::clone::Clone for #name {
                fn clone(&self) -> Self {
                    *self
                }
            }
            #[automatically_derived]
            impl #core::marker::Copy for #name { }
        }
    };

    let impl_with_repr = if info.has_explicit_repr() {
        quote! {
            #[automatically_derived]
            unsafe impl #enumset::EnumSetTypeWithRepr for #name {
                type Repr = #repr;
            }
        }
    } else {
        quote! {}
    };

    let inherent_impl_blocks = match info.internal_repr() {
        InternalRepr::U8
        | InternalRepr::U16
        | InternalRepr::U32
        | InternalRepr::U64
        | InternalRepr::U128 => {
            let value_as_repr_mask = if is_uninhabited {
                quote! { 0 } // impossible anyway
            } else {
                quote! { 1 << value as #repr }
            };

            quote! {
                #[automatically_derived]
                #[doc(hidden)]
                impl __EnumSetInitHelper {
                    pub const fn const_only(&self, value: #name) -> #enumset::EnumSet<#name> {
                        #enumset::EnumSet { __priv_repr: #value_as_repr_mask }
                    }
                }

                #[automatically_derived]
                #[doc(hidden)]
                impl __EnumSetOpHelper {
                    pub const fn const_union(
                        &self,
                        chain_a: #enumset::EnumSet<#name>,
                        chain_b: #enumset::EnumSet<#name>,
                    ) -> #enumset::EnumSet<#name> {
                        #enumset::EnumSet {
                            __priv_repr: chain_a.__priv_repr | chain_b.__priv_repr,
                        }
                    }

                    pub const fn const_intersection(
                        &self,
                        chain_a: #enumset::EnumSet<#name>,
                        chain_b: #enumset::EnumSet<#name>,
                    ) -> #enumset::EnumSet<#name> {
                        #enumset::EnumSet {
                            __priv_repr: chain_a.__priv_repr & chain_b.__priv_repr,
                        }
                    }

                    pub const fn const_symmetric_difference(
                        &self,
                        chain_a: #enumset::EnumSet<#name>,
                        chain_b: #enumset::EnumSet<#name>,
                    ) -> #enumset::EnumSet<#name> {
                        #enumset::EnumSet {
                            __priv_repr: chain_a.__priv_repr ^ chain_b.__priv_repr,
                        }
                    }

                    pub const fn const_complement(
                        &self,
                        chain: #enumset::EnumSet<#name>,
                    ) -> #enumset::EnumSet<#name> {
                        let mut all = #enumset::EnumSet::<#name>::all();
                        #enumset::EnumSet {
                            __priv_repr: !chain.__priv_repr & all.__priv_repr,
                        }
                    }
                }
            }
        }
        InternalRepr::Array(size) => {
            quote! {
                #[automatically_derived]
                #[doc(hidden)]
                impl __EnumSetInitHelper {
                    pub const fn const_only(&self, value: #name) -> #enumset::EnumSet<#name> {
                        let mut set = #enumset::EnumSet::<#name> {
                            __priv_repr: #internal::ArrayRepr::<{ #size }>([0; #size]),
                        };
                        let bit = value as u32;
                        let (idx, bit) = (bit as usize / 64, bit % 64);
                        set.__priv_repr.0[idx] |= 1u64 << bit;
                        set
                    }
                }

                #[automatically_derived]
                #[doc(hidden)]
                impl __EnumSetOpHelper {
                    pub const fn const_union(
                        &self,
                        mut chain_a: #enumset::EnumSet<#name>,
                        chain_b: #enumset::EnumSet<#name>,
                    ) -> #enumset::EnumSet<#name> {
                        let mut i = 0;
                        while i < #size {
                            chain_a.__priv_repr.0[i] |= chain_b.__priv_repr.0[i];
                            i += 1;
                        }
                        chain_a
                    }

                    pub const fn const_intersection(
                        &self,
                        mut chain_a: #enumset::EnumSet<#name>,
                        chain_b: #enumset::EnumSet<#name>,
                    ) -> #enumset::EnumSet<#name> {
                        let mut i = 0;
                        while i < #size {
                            chain_a.__priv_repr.0[i] &= chain_b.__priv_repr.0[i];
                            i += 1;
                        }
                        chain_a
                    }

                    pub const fn const_symmetric_difference(
                        &self,
                        mut chain_a: #enumset::EnumSet<#name>,
                        chain_b: #enumset::EnumSet<#name>,
                    ) -> #enumset::EnumSet<#name> {
                        let mut i = 0;
                        while i < #size {
                            chain_a.__priv_repr.0[i] ^= chain_b.__priv_repr.0[i];
                            i += 1;
                        }
                        chain_a
                    }

                    pub const fn const_complement(
                        &self,
                        mut chain: #enumset::EnumSet<#name>,
                    ) -> #enumset::EnumSet<#name> {
                        let mut all = #enumset::EnumSet::<#name>::all();
                        let mut i = 0;
                        while i < #size {
                            let new = !chain.__priv_repr.0[i] & all.__priv_repr.0[i];
                            chain.__priv_repr.0[i] = new;
                            i += 1;
                        }
                        chain
                    }
                }
            }
        }
    };

    let mut generated_warnings = SynTokenStream::new();
    for (span, warning) in &info.warnings {
        generated_warnings.extend(quote_spanned! {
            *span => {
                #[deprecated(note = #warning)]
                #[allow(non_upper_case_globals)]
                const _w: () = ();
                let _ = _w;
            }
        });
    }

    let bit_width = info.bit_width();
    let variant_count = info.variants.len() as u32;
    let vis = &info.vis;
    quote! {
        const _: () = {
            #[automatically_derived]
            #[doc(hidden)]
            #vis struct __EnumSetInitHelper;

            #[automatically_derived]
            #[doc(hidden)]
            #vis struct __EnumSetOpHelper;

            #[automatically_derived]
            unsafe impl #internal::EnumSetTypePrivate for #name {
                type Repr = #repr;
                const ALL_BITS: Self::Repr = #all_variants;
                const BIT_WIDTH: u32 = #bit_width;
                const VARIANT_COUNT: u32 = #variant_count;

                #into_impl

                #internal::__if_serde! {
                    #serde_ops
                }
            }

            #[automatically_derived]
            unsafe impl #internal::EnumSetConstHelper for #name {
                type ConstInitHelper = __EnumSetInitHelper;
                const CONST_INIT_HELPER: __EnumSetInitHelper = __EnumSetInitHelper;
                type ConstOpHelper = __EnumSetOpHelper;
                const CONST_OP_HELPER: __EnumSetOpHelper = __EnumSetOpHelper;
            }

            #[automatically_derived]
            unsafe impl #enumset::EnumSetType for #name { }

            #impl_with_repr
            #super_impls
            #ops
            #inherent_impl_blocks

            fn __enumset_derive__generated_warnings() {
                #generated_warnings
            }
        };
    }
}
