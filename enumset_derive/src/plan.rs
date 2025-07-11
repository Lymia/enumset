//! This module handles parsing the input enum type, and planning the final representation for the
//! bitset.

use crate::error;
use darling::util::SpannedValue;
use darling::FromDeriveInput;
use proc_macro2::{Ident, Span};
use std::collections::HashSet;
use syn::spanned::Spanned;
use syn::{Data, DeriveInput, Expr, ExprLit, ExprUnary, Fields, Lit, UnOp, Variant, Visibility};

/// Decodes the custom attributes for our custom derive.
#[derive(FromDeriveInput, Default)]
#[darling(attributes(enumset), default)]
struct EnumsetAttrs {
    no_ops: bool,
    no_super_impls: bool,
    #[darling(default)]
    repr: SpannedValue<Option<String>>,
    #[darling(default)]
    serialize_repr: SpannedValue<Option<String>>,
    serialize_deny_unknown: bool,
    #[darling(default)]
    crate_name: Option<String>,

    // legacy options
    serialize_as_list: SpannedValue<bool>, // replaced with serialize_repr
    serialize_as_map: SpannedValue<bool>,  // replaced with serialize_repr
}

/// The internal representation of an enumset.
#[derive(Copy, Clone)]
pub enum InternalRepr {
    /// internal repr: `u8`
    U8,
    /// internal repr: `u16`
    U16,
    /// internal repr: `u32`
    U32,
    /// internal repr: `u64`
    U64,
    /// internal repr: `u128`
    U128,
    /// internal repr: `[u64; size]`
    Array(usize),
}
impl InternalRepr {
    /// Determines the number of variants supported by this repr.
    fn supported_variants(&self) -> usize {
        match self {
            InternalRepr::U8 => 8,
            InternalRepr::U16 => 16,
            InternalRepr::U32 => 32,
            InternalRepr::U64 => 64,
            InternalRepr::U128 => 128,
            InternalRepr::Array(size) => size * 64,
        }
    }
}

/// The serde representation of the enumset.
#[derive(Copy, Clone)]
pub enum SerdeRepr {
    /// serde type: `u8`
    U8,
    /// serde type: `u16`
    U16,
    /// serde type: `u32`
    U32,
    /// serde type: `u64`
    U64,
    /// serde type: `u128`
    U128,
    /// serde type: list of `T`
    List,
    /// serde type: map of `T` to `bool`
    Map,
    /// serde type: list of `u64`
    Array,
}
impl SerdeRepr {
    /// Determines the number of variants supported by this repr.
    fn supported_variants(&self) -> Option<usize> {
        match self {
            SerdeRepr::U8 => Some(8),
            SerdeRepr::U16 => Some(16),
            SerdeRepr::U32 => Some(32),
            SerdeRepr::U64 => Some(64),
            SerdeRepr::U128 => Some(128),
            SerdeRepr::List => None,
            SerdeRepr::Map => None,
            SerdeRepr::Array => None,
        }
    }
}

/// An variant in the enum set type.
pub struct EnumSetValue {
    /// The name of the variant.
    pub name: Ident,
    /// The discriminant of the variant.
    pub variant_repr: u32,
}

/// Stores information about the enum set type.
#[allow(dead_code)]
pub struct EnumSetInfo {
    /// The name of the enum.
    pub name: Ident,
    /// The crate name to use.
    pub crate_name: Option<Ident>,
    /// The numeric type to represent the `EnumSet` as in memory.
    explicit_internal_repr: Option<InternalRepr>,
    /// Forces the internal numeric type of the `EnumSet` to be an array.
    internal_repr_force_array: bool,
    /// The numeric type to serialize the enum as.
    explicit_serde_repr: Option<SerdeRepr>,
    /// A list of variants in the enum.
    pub variants: Vec<EnumSetValue>,
    /// Visbility
    pub vis: Visibility,

    /// The highest encountered variant discriminant.
    max_discrim: u32,
    /// The span of the highest encountered variant.
    max_discrim_span: Option<Span>,
    /// The current variant discriminant. Used to track, e.g. `A=10,B,C`.
    cur_discrim: u32,
    /// A list of variant names that are already in use.
    used_variant_names: HashSet<String>,
    /// A list of variant discriminants that are already in use.
    used_discriminants: HashSet<u32>,

    /// Avoid generating operator overloads on the enum type.
    pub no_ops: bool,
    /// Avoid generating implementations for `Clone`, `Copy`, `Eq`, and `PartialEq`.
    pub no_super_impls: bool,
    /// Disallow unknown bits while deserializing the enum.
    pub serialize_deny_unknown: bool,

    /// List of warnings for the enumset.
    pub warnings: Vec<(Span, &'static str)>,
}
impl EnumSetInfo {
    fn new(input: &DeriveInput, attrs: &EnumsetAttrs) -> EnumSetInfo {
        EnumSetInfo {
            name: input.ident.clone(),
            crate_name: attrs
                .crate_name
                .as_ref()
                .map(|x| Ident::new(x, Span::call_site())),
            explicit_internal_repr: None,
            internal_repr_force_array: false,
            explicit_serde_repr: None,
            variants: Vec::new(),
            vis: input.vis.clone(),
            max_discrim: 0,
            max_discrim_span: None,
            cur_discrim: 0,
            used_variant_names: HashSet::new(),
            used_discriminants: HashSet::new(),
            no_ops: attrs.no_ops,
            no_super_impls: attrs.no_super_impls,
            serialize_deny_unknown: attrs.serialize_deny_unknown,
            warnings: vec![],
        }
    }

    /// Explicits sets the serde representation of the enumset from a string.
    fn push_serialize_repr(&mut self, span: Span, ty: &str) -> syn::Result<()> {
        match ty {
            "u8" => self.explicit_serde_repr = Some(SerdeRepr::U8),
            "u16" => self.explicit_serde_repr = Some(SerdeRepr::U16),
            "u32" => self.explicit_serde_repr = Some(SerdeRepr::U32),
            "u64" => self.explicit_serde_repr = Some(SerdeRepr::U64),
            "u128" => self.explicit_serde_repr = Some(SerdeRepr::U128),
            "list" => self.explicit_serde_repr = Some(SerdeRepr::List),
            "map" => self.explicit_serde_repr = Some(SerdeRepr::Map),
            "array" => self.explicit_serde_repr = Some(SerdeRepr::Array),
            _ => error(span, format!("`{ty}` is not a valid serialized representation."))?,
        }
        Ok(())
    }

    /// Explicitly sets the representation of the enumset from a string.
    fn push_repr(&mut self, span: Span, ty: &str) -> syn::Result<()> {
        match ty {
            "u8" => self.explicit_internal_repr = Some(InternalRepr::U8),
            "u16" => self.explicit_internal_repr = Some(InternalRepr::U16),
            "u32" => self.explicit_internal_repr = Some(InternalRepr::U32),
            "u64" => self.explicit_internal_repr = Some(InternalRepr::U64),
            "u128" => self.explicit_internal_repr = Some(InternalRepr::U128),
            "array" => self.internal_repr_force_array = true,
            _ => error(span, format!("`{ty}` is not a valid internal enumset representation."))?,
        }
        Ok(())
    }

    /// Adds a variant to the enumset.
    fn push_variant(&mut self, variant: &Variant) -> syn::Result<()> {
        if self.used_variant_names.contains(&variant.ident.to_string()) {
            error(variant.span(), "Duplicated variant name.")
        } else if let Fields::Unit = variant.fields {
            // Parse the discriminant.
            if let Some((_, expr)) = &variant.discriminant {
                if let Expr::Lit(ExprLit { lit: Lit::Int(i), .. }) = expr {
                    match i.base10_parse() {
                        Ok(val) => self.cur_discrim = val,
                        Err(_) => error(expr.span(), "Enum discriminants must fit into `u32`.")?,
                    }
                } else if let Expr::Unary(ExprUnary { op: UnOp::Neg(_), .. }) = expr {
                    error(expr.span(), "Enum discriminants must not be negative.")?;
                } else {
                    error(variant.span(), "Enum discriminants must be literal expressions.")?;
                }
            }

            // Validate the discriminant.
            let discriminant = self.cur_discrim;
            if discriminant >= 0xFFFFFFC0 {
                error(variant.span(), "Maximum discriminant allowed is `0xFFFFFFBF`.")?;
            }
            if self.used_discriminants.contains(&discriminant) {
                error(variant.span(), "Duplicated enum discriminant.")?;
            }

            // Add the variant to the info.
            self.cur_discrim += 1;
            if discriminant > self.max_discrim {
                self.max_discrim = discriminant;
                self.max_discrim_span = Some(variant.span());
            }
            self.variants
                .push(EnumSetValue { name: variant.ident.clone(), variant_repr: discriminant });
            self.used_variant_names.insert(variant.ident.to_string());
            self.used_discriminants.insert(discriminant);

            Ok(())
        } else {
            error(variant.span(), "`#[derive(EnumSetType)]` can only be used on fieldless enums.")
        }
    }

    /// Returns the actual internal representation of the set.
    pub fn internal_repr(&self) -> InternalRepr {
        match self.explicit_internal_repr {
            Some(x) => x,
            None => match self.max_discrim {
                x if x < 8 && !self.internal_repr_force_array => InternalRepr::U8,
                x if x < 16 && !self.internal_repr_force_array => InternalRepr::U16,
                x if x < 32 && !self.internal_repr_force_array => InternalRepr::U32,
                x if x < 64 && !self.internal_repr_force_array => InternalRepr::U64,
                x => InternalRepr::Array((x as usize + 64) / 64),
            },
        }
    }

    /// Returns whether this enumset has an explicit internal representation.
    pub fn has_explicit_repr(&self) -> bool {
        self.explicit_internal_repr.is_some()
    }

    /// Returns the actual serde representation of the set.
    pub fn serde_repr(&self) -> SerdeRepr {
        match self.explicit_serde_repr {
            Some(x) => x,
            None => match self.max_discrim {
                x if x < 8 => SerdeRepr::U8,
                x if x < 16 => SerdeRepr::U16,
                x if x < 32 => SerdeRepr::U32,
                x if x < 64 => SerdeRepr::U64,
                x if x < 128 => SerdeRepr::U128,
                _ => SerdeRepr::Array,
            },
        }
    }

    /// Returns the number of bits this enumset takes.
    pub fn bit_width(&self) -> u32 {
        self.max_discrim + 1
    }

    /// Validate the enumset type.
    fn validate(&self) -> syn::Result<()> {
        // Gets the span of the maximum value.
        let largest_discriminant_span = match &self.max_discrim_span {
            Some(x) => *x,
            None => Span::call_site(),
        };

        // Check if all bits of the bitset can fit in the memory representation, if one was given.
        if self.internal_repr().supported_variants() <= self.max_discrim as usize {
            error(
                largest_discriminant_span,
                "`repr` is too small to contain the largest discriminant.",
            )?;
        }

        // Check if all bits of the bitset can fit in the serialization representation.
        if let Some(supported_variants) = self.serde_repr().supported_variants() {
            if supported_variants <= self.max_discrim as usize {
                error(
                    largest_discriminant_span,
                    "`serialize_repr` is too small to contain the largest discriminant.",
                )?;
            }
        }

        Ok(())
    }

    /// Returns a bitmask of all variants in the set.
    pub fn variant_map(&self) -> Vec<u64> {
        let mut vec = vec![0];
        for variant in &self.variants {
            let (idx, bit) = (variant.variant_repr as usize / 64, variant.variant_repr % 64);
            while idx >= vec.len() {
                vec.push(0);
            }
            vec[idx] |= 1u64 << bit;
        }
        vec
    }
}

pub fn plan_for_enum(input: DeriveInput) -> syn::Result<EnumSetInfo> {
    let attrs: EnumsetAttrs = EnumsetAttrs::from_derive_input(&input)?;

    if !input.generics.params.is_empty() {
        error(
            input.generics.span(),
            "`#[derive(EnumSetType)]` cannot be used on enums with type parameters.",
        )
    } else if let Data::Enum(data) = &input.data {
        let mut info = EnumSetInfo::new(&input, &attrs);

        // Check enum repr
        for attr in &input.attrs {
            if attr.path().is_ident("repr") {
                let meta: Ident = attr.parse_args()?;
                match meta.to_string().as_str() {
                    "C" | "Rust" => {}
                    "u8" | "u16" | "u32" | "u64" | "u128" | "usize" => {}
                    "i8" | "i16" | "i32" | "i64" | "i128" | "isize" => {}
                    x => error(
                        attr.span(),
                        format!("`#[repr({x})]` cannot be used on enumset variants."),
                    )?,
                }
            }
        }

        // Parse internal representations
        if let Some(repr) = &*attrs.repr {
            info.push_repr(attrs.repr.span(), repr)?;
        }

        // Parse serialization representations
        if let Some(serialize_repr) = &*attrs.serialize_repr {
            info.push_serialize_repr(attrs.serialize_repr.span(), serialize_repr)?;
        }
        if *attrs.serialize_as_map {
            info.explicit_serde_repr = Some(SerdeRepr::Map);
            info.warnings.push((
                attrs.serialize_as_map.span(),
                "#[enumset(serialize_as_map)] is deprecated. \
                 Use `#[enumset(serialize_repr = \"map\")]` instead.",
            ));
        }
        if *attrs.serialize_as_list {
            // in old versions, serialize_as_list will override serialize_as_map
            info.explicit_serde_repr = Some(SerdeRepr::List);
            info.warnings.push((
                attrs.serialize_as_list.span(),
                "#[enumset(serialize_as_list)] is deprecated. \
                 Use `#[enumset(serialize_repr = \"list\")]` instead.",
            ));
        }
        #[cfg(feature = "std_deprecation_warning")]
        {
            info.warnings.push((
                input.span(),
                "feature = \"std\" is depercated. If you rename `enumset`, use \
                 feature = \"proc-macro-crate\" instead. If you don't, remove the feature.",
            ));
        }
        #[cfg(feature = "serde2_deprecation_warning")]
        {
            info.warnings.push((
                input.span(),
                "feature = \"serde2\" was never valid and did nothing. Please remove the feature.",
            ));
        }

        // Parse enum variants
        for variant in &data.variants {
            info.push_variant(variant)?;
        }

        // Validate the enumset
        info.validate()?;

        // Generates the actual `EnumSetType` implementation
        Ok(info)
    } else {
        error(input.span(), "`#[derive(EnumSetType)]` may only be used on enums")
    }
}
