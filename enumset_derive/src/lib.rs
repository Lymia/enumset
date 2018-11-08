#![recursion_limit="128"]
#![cfg_attr(feature = "nightly", feature(proc_macro_diagnostic))]

extern crate syn;
extern crate proc_macro;
extern crate proc_macro2;
extern crate quote;

use self::proc_macro::{TokenStream, TokenTree, Literal};

use proc_macro2::{TokenStream as SynTokenStream};
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

fn enum_set_type_impl(name: &Ident, all_variants: u128, repr: Ident) -> SynTokenStream {
    let typed_enumset = quote!(::enumset::EnumSet<#name>);
    let core = quote!(::enumset::internal::core);

    // proc_macro2 does not support creating u128 literals.
    let all_variants_tt = TokenTree::Literal(Literal::u128_unsuffixed(all_variants));
    let all_variants_tt = SynTokenStream::from(TokenStream::from(all_variants_tt));

    quote! {
        unsafe impl ::enumset::EnumSetType for #name {
            type Repr = #repr;
            const ALL_BITS: Self::Repr = #all_variants_tt;

            fn enum_into_u8(self) -> u8 {
                self as u8
            }
            unsafe fn enum_from_u8(val: u8) -> Self {
                #core::mem::transmute(val)
            }
        }

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
}

#[proc_macro_derive(EnumSetType)]
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

            enum_set_type_impl(&input.ident, all_variants, repr).into()
        }
    } else {
        error(input.span(), "`#[derive(EnumSetType)]` may only be used on enums")
    }
}
