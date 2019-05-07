#![recursion_limit="256"]
#![cfg_attr(feature = "nightly", feature(proc_macro_diagnostic))]

extern crate proc_macro;

use darling::*;
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
    name: &Ident, all_variants: u128, repr: Ident, attrs: EnumsetAttrs,
) -> SynTokenStream {
    let typed_enumset = quote!(::enumset::EnumSet<#name>);
    let core = quote!(::enumset::internal::core);
    #[cfg(feature = "serde")]
    let serde = quote!(::enumset::internal::serde);

    // proc_macro2 does not support creating u128 literals.
    let all_variants = Literal::u128_unsuffixed(all_variants);

    let ops = if attrs.no_ops {
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

    #[cfg(feature = "serde")]
    let serde_ops = if attrs.serialize_as_list {
        let expecting_str = format!("a list of {}", name);
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
        let serialize_repr = attrs.serialize_repr.as_ref()
            .map(|x| Ident::new(&x, Span::call_site()))
            .unwrap_or(repr.clone());
        let check_unknown = if attrs.serialize_deny_unknown {
            quote! {
                if value & !#all_variants != 0 {
                    use #serde::de::Error;
                    let unexpected = #serde::de::Unexpected::Unsigned(value as u64);
                    return #core::prelude::v1::Err(
                        D::Error::custom("enumset contains unknown bits")
                    )
                }
            }
        } else {
            quote! { }
        };
        quote! {
            fn serialize<S: #serde::Serializer>(
                set: ::enumset::EnumSet<#name>, ser: S,
            ) -> #core::result::Result<S::Ok, S::Error> {
                use #serde::Serialize;
                #serialize_repr::serialize(&(set.__enumset_underlying as #serialize_repr), ser)
            }
            fn deserialize<'de, D: #serde::Deserializer<'de>>(
                de: D,
            ) -> #core::result::Result<::enumset::EnumSet<#name>, D::Error> {
                use #serde::Deserialize;
                let value = #serialize_repr::deserialize(de)?;
                #check_unknown
                #core::prelude::v1::Ok(::enumset::EnumSet {
                    __enumset_underlying: (value & #all_variants) as #repr,
                })
            }
        }
    };

    #[cfg(not(feature = "serde"))]
    let serde_ops = quote! { };

    quote! {
        unsafe impl ::enumset::internal::EnumSetTypePrivate for #name {
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

        unsafe impl ::enumset::EnumSetType for #name { }

        impl #core::cmp::PartialEq for #name {
            fn eq(&self, other: &Self) -> bool {
                (*self as u8) == (*other as u8)
            }
        }
        impl #core::cmp::Eq for #name { }
        impl #core::clone::Clone for #name {
            fn clone(&self) -> Self {
                *self
            }
        }
        impl #core::marker::Copy for #name { }

        #ops
    }
}

#[derive(FromDeriveInput, Default)]
#[darling(attributes(enumset), default)]
struct EnumsetAttrs {
    no_ops: bool,
    serialize_as_list: bool,
    serialize_deny_unknown: bool,
    #[darling(default)]
    serialize_repr: Option<String>,
}

#[proc_macro_derive(EnumSetType, attributes(enumset))]
pub fn derive_enum_set_type(input: TokenStream) -> TokenStream {
    let input: DeriveInput = parse_macro_input!(input);
    if let Data::Enum(data) = &input.data {
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

            let repr = Ident::new(if max_variant <= 7 {
                "u8"
            } else if max_variant <= 15 {
                "u16"
            } else if max_variant <= 31 {
                "u32"
            } else if max_variant <= 63 {
                "u64"
            } else if max_variant <= 127 {
                "u128"
            } else {
                panic!("max_variant > 127?")
            }, Span::call_site());

            let attrs: EnumsetAttrs = match EnumsetAttrs::from_derive_input(&input) {
                Ok(attrs) => attrs,
                Err(e) => return e.write_errors().into(),
            };

            match attrs.serialize_repr.as_ref().map(|x| x.as_str()) {
                Some("u8") => if max_variant > 7 {
                    return error(input.span(), "Too many variants for u8 serialization repr.")
                }
                Some("u16") => if max_variant > 15 {
                    return error(input.span(), "Too many variants for u16 serialization repr.")
                }
                Some("u32") => if max_variant > 31 {
                    return error(input.span(), "Too many variants for u32 serialization repr.")
                }
                Some("u64") => if max_variant > 63 {
                    return error(input.span(), "Too many variants for u64 serialization repr.")
                }
                Some("u128") => if max_variant > 127 {
                    return error(input.span(), "Too many variants for u128 serialization repr.")
                }
                None => { }
                Some(x) => return error(input.span(),
                                        &format!("{} is not a valid serialization repr.", x)),
            };

            enum_set_type_impl(&input.ident, all_variants, repr, attrs).into()
        }
    } else {
        error(input.span(), "`#[derive(EnumSetType)]` may only be used on enums")
    }
}
