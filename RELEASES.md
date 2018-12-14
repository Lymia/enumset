# Unreleased
* Added optional `serde` support.

# Version 0.3.14 (2018-11-09)

* Deprecated the `enum_set_type!` macro in favor of a custom derive.
* Added a way to avoid the automatic operator overloads derived from
  EnumSetTypes.
* Fixed a major issue that would have prevented `enumset` from being used in
  `#[no_std]` crates.
* Minimum required version is now 1.30.0+. There should be no more need to bump
  the minimum Rust version in the forseeable future.

# Prior versions

No release notes were kept for prior versions.