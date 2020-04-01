#![recursion_limit="256"]
#![cfg_attr(feature = "nightly", feature(proc_macro_diagnostic))]

// TODO: Read #[repr(...)] attributes.

extern crate proc_macro;

use darling::*;
use proc_macro::TokenStream;
use proc_macro2::{TokenStream as SynTokenStream, Literal};
use syn::{*, Result, Error};
use syn::export::Span;
use syn::spanned::Spanned;
use quote::*;

fn error<T>(span: Span, message: &str) -> Result<T> {
    Err(Error::new(span, message))
}

fn enum_set_type_impl(
    name: &Ident, all_variants: u128, repr: Ident, attrs: EnumsetAttrs, variants: Vec<Ident>,
    enum_repr: Ident,
) -> SynTokenStream {
    let is_uninhabited = variants.is_empty();
    let is_zst = variants.len() == 1;

    let typed_enumset = quote!(::enumset::EnumSet<#name>);
    let core = quote!(::enumset::internal::core_export);
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
                    ) -> #core::result::Result<Self::Value, A::Error> where
                        A: #serde::de::SeqAccess<'de>
                    {
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
                #serde::Serialize::serialize(&(set.__enumset_underlying as #serialize_repr), ser)
            }
            fn deserialize<'de, D: #serde::Deserializer<'de>>(
                de: D,
            ) -> #core::result::Result<::enumset::EnumSet<#name>, D::Error> {
                let value = <#serialize_repr as #serde::Deserialize>::deserialize(de)?;
                #check_unknown
                #core::prelude::v1::Ok(::enumset::EnumSet {
                    __enumset_underlying: (value & #all_variants) as #repr,
                })
            }
        }
    };

    #[cfg(not(feature = "serde"))]
    let serde_ops = quote! { };

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
        let variant = &variants[0];
        quote! {
            fn enum_into_u32(self) -> u32 {
                self as u32
            }
            unsafe fn enum_from_u32(val: u32) -> Self {
                #name::#variant
            }
        }
    } else {
        quote! {
            fn enum_into_u32(self) -> u32 {
                self as u32
            }
            unsafe fn enum_from_u32(val: u32) -> Self {
                #core::mem::transmute(val as #enum_repr)
            }
        }
    };

    let eq_impl = if is_uninhabited {
        quote!(panic!(concat!(stringify!(#name), " is uninhabited.")))
    } else {
        quote!((*self as u32) == (*other as u32))
    };

    quote! {
        unsafe impl ::enumset::internal::EnumSetTypePrivate for #name {
            type Repr = #repr;
            const ALL_BITS: Self::Repr = #all_variants;
            #into_impl
            #serde_ops
        }

        unsafe impl ::enumset::EnumSetType for #name { }

        impl #core::cmp::PartialEq for #name {
            fn eq(&self, other: &Self) -> bool {
                #eq_impl
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

fn derive_enum_set_type_impl(input: DeriveInput) -> Result<TokenStream> {
    if let Data::Enum(data) = &input.data {
        if !input.generics.params.is_empty() {
            error(
                input.generics.span(),
                "`#[derive(EnumSetType)]` cannot be used on enums with type parameters."
            )
        } else {
            let mut all_variants = 0u128;
            let mut max_variant = 0u32;
            let mut current_variant = 0u32;
            let mut has_manual_discriminant = false;
            let mut variants = Vec::new();

            for variant in &data.variants {
                if let Fields::Unit = variant.fields {
                    if let Some((_, expr)) = &variant.discriminant {
                        if let Expr::Lit(ExprLit { lit: Lit::Int(i), .. }) = expr {
                            current_variant = match i.base10_parse() {
                                Ok(val) => val,
                                Err(_) => error(
                                    expr.span(), "Could not parse discriminant as u32.",
                                )?,
                            };
                            has_manual_discriminant = true;
                        } else {
                            error(
                                variant.span(), "Unrecognized discriminant for variant."
                            )?;
                        }
                    }

                    if current_variant >= 128 {
                        let message = if has_manual_discriminant {
                            "`#[derive(EnumSetType)]` currently only supports \
                             enum discriminants up to 127."
                        } else {
                            "`#[derive(EnumSetType)]` currently only supports \
                             enums up to 128 variants."
                        };
                        error(variant.span(), message)?;
                    }

                    if all_variants & (1 << current_variant as u128) != 0 {
                        error(
                            variant.span(),
                            &format!("Duplicate enum discriminant: {}", current_variant)
                        )?;
                    }
                    all_variants |= 1 << current_variant as u128;
                    if current_variant > max_variant {
                        max_variant = current_variant
                    }

                    variants.push(variant.ident.clone());
                    current_variant += 1;
                } else {
                    error(
                        variant.span(),
                        "`#[derive(EnumSetType)]` can only be used on C-like enums."
                    )?;
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
                Err(e) => return Ok(e.write_errors().into()),
            };

            let mut enum_repr = None;
            for attr in &input.attrs {
                if attr.path.is_ident(&Ident::new("repr", Span::call_site())) {
                    let meta: Ident = attr.parse_args()?;
                    if enum_repr.is_some() {
                        error(attr.span(), "Cannot duplicate #[repr(...)] annotations.")?;
                    }
                    let repr_max_variant = match meta.to_string().as_str() {
                        "u8" => 0xFF,
                        "u16" => 0xFFFF,
                        "u32" => 0xFFFFFFFF,
                        _ => error(attr.span(), "Only `u8`, `u16` and `u32` reprs are supported.")?,
                    };
                    if max_variant > repr_max_variant {
                        error(attr.span(), "A variant of this enum overflows its repr.")?;
                    }
                    enum_repr = Some(meta);
                }
            }
            let enum_repr = enum_repr.unwrap_or_else(|| if max_variant < 0x100 {
                Ident::new("u8", Span::call_site())
            } else if max_variant < 0x10000 {
                Ident::new("u16", Span::call_site())
            } else {
                Ident::new("u32", Span::call_site())
            });

            match attrs.serialize_repr.as_ref().map(|x| x.as_str()) {
                Some("u8") => if max_variant > 7 {
                    error(input.span(), "Too many variants for u8 serialization repr.")?;
                }
                Some("u16") => if max_variant > 15 {
                    error(input.span(), "Too many variants for u16 serialization repr.")?;
                }
                Some("u32") => if max_variant > 31 {
                    error(input.span(), "Too many variants for u32 serialization repr.")?;
                }
                Some("u64") => if max_variant > 63 {
                    error(input.span(), "Too many variants for u64 serialization repr.")?;
                }
                Some("u128") => if max_variant > 127 {
                    error(input.span(), "Too many variants for u128 serialization repr.")?;
                }
                None => { }
                Some(x) => error(
                    input.span(), &format!("{} is not a valid serialization repr.", x)
                )?,
            };

            Ok(enum_set_type_impl(
                &input.ident, all_variants, repr, attrs, variants, enum_repr,
            ).into())
        }
    } else {
        error(input.span(), "`#[derive(EnumSetType)]` may only be used on enums")
    }
}

#[proc_macro_derive(EnumSetType, attributes(enumset))]
pub fn derive_enum_set_type(input: TokenStream) -> TokenStream {
    let input: DeriveInput = parse_macro_input!(input);
    match derive_enum_set_type_impl(input) {
        Ok(v) => v,
        Err(e) => e.to_compile_error().into(),
    }
}