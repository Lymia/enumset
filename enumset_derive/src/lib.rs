#![recursion_limit="256"]
#![cfg_attr(feature = "nightly", feature(proc_macro_diagnostic))]

extern crate syn;
extern crate proc_macro;
extern crate proc_macro2;
extern crate quote;

use proc_macro::TokenStream;
use proc_macro2::{TokenStream as SynTokenStream, Literal};
use syn::*;
use syn::export::Span;
use syn::spanned::Spanned;
use quote::*;

#[cfg(feature = "nightly")]
fn error(span: Span, data: &str) -> TokenStream {
    span.unstable().error(data).emit();
    TokenStream::new()
}

#[cfg(not(feature = "nightly"))]
fn error(_: Span, data: &str) -> TokenStream {
    panic!("{}", data)
}

fn enum_set_type_impl(
    name: &Ident, all_variants: u128, repr: Ident,
    no_ops: bool, no_derives: bool, _serialize_as_list: bool,
) -> SynTokenStream {
    let typed_enumset = quote!(::enumset::EnumSet<#name>);
    let core = quote!(::enumset::internal::core);
    #[cfg(feature = "serde")]
    let serde = quote!(::enumset::internal::serde);

    // proc_macro2 does not support creating u128 literals.
    let all_variants = Literal::u128_unsuffixed(all_variants);

    let ops = if no_ops {
        quote! {}
    } else {
        quote! {
            impl <O : Into<#typed_enumset>> #core::ops::Sub<O> for #name {
                type Output = #typed_enumset;
                fn sub(self, other: O) -> Self::Output {
                    ::enumset::EnumSet::only(self) - other.into()
                }
            }
            impl <O : Into<#typed_enumset>> #core::ops::BitAnd<O> for #name {
                type Output = #typed_enumset;
                fn bitand(self, other: O) -> Self::Output {
                    ::enumset::EnumSet::only(self) & other.into()
                }
            }
            impl <O : Into<#typed_enumset>> #core::ops::BitOr<O> for #name {
                type Output = #typed_enumset;
                fn bitor(self, other: O) -> Self::Output {
                    ::enumset::EnumSet::only(self) | other.into()
                }
            }
            impl <O : Into<#typed_enumset>> #core::ops::BitXor<O> for #name {
                type Output = #typed_enumset;
                fn bitxor(self, other: O) -> Self::Output {
                    ::enumset::EnumSet::only(self) ^ other.into()
                }
            }
            impl #core::ops::Not for #name {
                type Output = #typed_enumset;
                fn not(self) -> Self::Output {
                    !::enumset::EnumSet::only(self)
                }
            }
            impl #core::cmp::PartialEq<#typed_enumset> for #name {
                fn eq(&self, other: &#typed_enumset) -> bool {
                    ::enumset::EnumSet::only(*self) == *other
                }
            }
        }
    };

    let derives = if no_derives {
        quote! {}
    } else {
        quote! {
            impl #core::cmp::PartialOrd for #name {
                fn partial_cmp(&self, other: &Self) -> #core::option::Option<#core::cmp::Ordering> {
                    (*self as u8).partial_cmp(&(*other as u8))
                }
            }
            impl #core::cmp::Ord for #name {
                fn cmp(&self, other: &Self) -> #core::cmp::Ordering {
                    (*self as u8).cmp(&(*other as u8))
                }
            }
            impl #core::cmp::PartialEq for #name {
                fn eq(&self, other: &Self) -> bool {
                    (*self as u8) == (*other as u8)
                }
            }
            impl #core::cmp::Eq for #name { }
            impl #core::hash::Hash for #name {
                fn hash<H: #core::hash::Hasher>(&self, state: &mut H) {
                    state.write_u8(*self as u8)
                }
            }
            impl #core::clone::Clone for #name {
                fn clone(&self) -> Self {
                    *self
                }
            }
            impl #core::marker::Copy for #name { }
        }
    };

    #[cfg(feature = "serde")]
    let expecting_str = format!("a list of {}", name);

    #[cfg(feature = "serde")]
    let serde_ops = if _serialize_as_list {
        quote! {
            fn serialize<S: #serde::Serializer>(
                set: ::enumset::EnumSet<#name>, ser: S,
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
            ) -> #core::result::Result<::enumset::EnumSet<#name>, D::Error> {
                struct Visitor;
                impl <'de> #serde::de::Visitor<'de> for Visitor {
                    type Value = ::enumset::EnumSet<#name>;
                    fn expecting(
                        &self, formatter: &mut #core::fmt::Formatter,
                    ) -> #core::fmt::Result {
                        write!(formatter, #expecting_str)
                    }
                    fn visit_seq<A>(
                        mut self, mut seq: A,
                    ) -> Result<Self::Value, A::Error> where A: #serde::de::SeqAccess<'de> {
                        let mut accum = ::enumset::EnumSet::<#name>::new();
                        while let #core::prelude::v1::Some(val) = seq.next_element::<#name>()? {
                            accum |= val;
                        }
                        #core::prelude::v1::Ok(accum)
                    }
                }
                de.deserialize_seq(Visitor)
            }
        }
    } else {
        quote! {
            fn serialize<S: #serde::Serializer>(
                set: ::enumset::EnumSet<#name>, ser: S,
            ) -> #core::result::Result<S::Ok, S::Error> {
                use #serde::Serialize;
                <#name as ::enumset::EnumSetType>::Repr::serialize(&set.__enumset_underlying, ser)
            }
            fn deserialize<'de, D: #serde::Deserializer<'de>>(
                de: D,
            ) -> #core::result::Result<::enumset::EnumSet<#name>, D::Error> {
                use #serde::Deserialize;
                #core::prelude::v1::Ok(::enumset::EnumSet {
                    __enumset_underlying: <#name as ::enumset::EnumSetType>::Repr::deserialize(de)?,
                })
            }
        }
    };

    #[cfg(not(feature = "serde"))]
    let serde_ops = quote! { };

    quote! {
        unsafe impl ::enumset::EnumSetType for #name {
            type Repr = #repr;
            const ALL_BITS: Self::Repr = #all_variants;

            fn enum_into_u8(self) -> u8 {
                self as u8
            }
            unsafe fn enum_from_u8(val: u8) -> Self {
                #core::mem::transmute(val)
            }

            #serde_ops
        }

        #ops
        #derives
    }
}

#[proc_macro_derive(EnumSetType,
                    attributes(enumset_no_ops, enumset_no_derives, enumset_serialize_as_list))]
pub fn derive_enum_set_type(input: TokenStream) -> TokenStream {
    let input: DeriveInput = parse_macro_input!(input);
    if let Data::Enum(data) = input.data {
        if !input.generics.params.is_empty() {
            error(input.generics.span(),
                  "`#[derive(EnumSetType)]` cannot be used on enums with type parameters.")
        } else {
            let mut all_variants = 0u128;
            let mut max_variant = 0;
            let mut current_variant = 0;
            let mut has_manual_discriminant = false;

            for variant in &data.variants {
                if let Fields::Unit = variant.fields {
                    if let Some((_, expr)) = &variant.discriminant {
                        if let Expr::Lit(ExprLit { lit: Lit::Int(i), .. }) = expr {
                            current_variant = i.value();
                            has_manual_discriminant = true;
                        } else {
                            return error(variant.span(), "Unrecognized discriminant for variant.")
                        }
                    }

                    if current_variant >= 128 {
                        let message = if has_manual_discriminant {
                            "`#[derive(EnumSetType)]` only supports enum discriminants up to 127."
                        } else {
                            "`#[derive(EnumSetType)]` only supports enums up to 128 variants."
                        };
                        return error(variant.span(), message)
                    }

                    if all_variants & (1 << current_variant) != 0 {
                        return error(variant.span(),
                                     &format!("Duplicate enum discriminant: {}", current_variant))
                    }
                    all_variants |= 1 << current_variant;
                    if current_variant > max_variant {
                        max_variant = current_variant
                    }

                    current_variant += 1;
                } else {
                    return error(variant.span(),
                                 "`#[derive(EnumSetType)]` can only be used on C-like enums.")
                }
            }

            let repr = Ident::new(if max_variant <= 8 {
                "u8"
            } else if max_variant <= 16 {
                "u16"
            } else if max_variant <= 32 {
                "u32"
            } else if max_variant <= 64 {
                "u64"
            } else if max_variant <= 128 {
                "u128"
            } else {
                panic!("max_variant > 128?")
            }, Span::call_site());

            let mut no_ops = false;
            let mut no_derives = false;
            let mut serialize_as_list = false;

            for attr in &input.attrs {
                let span = attr.span();
                let Attribute { tts, path: Path { segments, ..}, .. } = attr;

                if segments.len() == 1 {
                    let name = segments[0].ident.to_string();
                    let mut is_attr = false;
                    match name.as_str() {
                        "enumset_no_ops" => {
                            no_ops = true;
                            is_attr = true;
                        }
                        "enumset_no_derives" => {
                            no_derives = true;
                            is_attr = true;
                        }
                        "enumset_serialize_as_list" => {
                            serialize_as_list = true;
                            is_attr = true;
                        }
                        _ => { }
                    }
                    if is_attr && !tts.is_empty() {
                        return error(span, &format!("`#[{}]` takes no arguments.", name))
                    }
                }
            }

            enum_set_type_impl(
                &input.ident, all_variants, repr, no_ops, no_derives, serialize_as_list,
            ).into()
        }
    } else {
        error(input.span(), "`#[derive(EnumSetType)]` may only be used on enums")
    }
}
