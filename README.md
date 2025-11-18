# enumset

[![Build Status](https://github.com/Lymia/enumset/actions/workflows/test.yml/badge.svg)](https://github.com/Lymia/enumset/actions/workflows/test.yml)
[![Latest Version](https://img.shields.io/crates/v/enumset.svg)](https://crates.io/crates/enumset)
![Requires rustc 1.61+](https://img.shields.io/badge/rustc-1.61+-red.svg)
[![Rust Documentation](https://img.shields.io/badge/api-rustdoc-blue.svg)](https://docs.rs/enumset)

A library for defining enums that can be used in compact bit sets. It supports
`serde` and `#[no_std]` environments, and has basic support for using EnumSets
in constants.

See [the documentation](https://docs.rs/enumset) for more information.

# Comparison to other popular crates

 * Unlike other popular crates, `enumset` does not interfere with the
   definition of your flag enum.  Instead, it uses a derive macro.
   (`bitflags` and `flagset` require the enum to be defined via a
   macro taking a custom input syntax.  `enumflags2` uses an attribute
   macro which massages the discriminants.)

 * Unlike other popular crates, `enumset` offers a variety of approaches
   for the `serde` integration.

 * Unlike `bitflags`, but like `enumflags2` and `flagset`
   `enumset` makes a type-level distinction between
   the enum (which has individual flags) and a *set* of flags.

 * Unlike other popular crates, by default `enumset` uses the enum
   discriminants as *bit indices* for the compact bitset
   representation, rather than flag values directly.
   So the discriminants do not need to be specially assigned.
   (Representation can be controlled with the `map =` option.)

# License

This project is licensed under either of

 * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or
   http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or
   http://opensource.org/licenses/MIT)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in enumset by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
