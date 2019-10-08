# Version 0.4.3 (2019-10-08)
* Implemented `Extend` and `FromIterator` for `EnumSet<T>`.

# Version 0.4.2 (2019-09-28)
* Fixed a bug preventing empty enums and enums with one value from compiling.

# Version 0.4.1 (2019-09-27)
* Fixed bug in `EnumSetIter::size_hint`.

# Version 0.4.0 (2019-05-06)
* Removed outdated macros and attributes.
* All attributes for the custom derive are now written as `#[enumset(...)]`
* Implemented `#[enumset(serialize_deny_unknown)]`
* Implemented `#[enumset(serialize_repr="...")]`
* `#[derive(EnumSetType)]` now only implements `Copy`, `Clone`, `PartialEq` and `Eq` automatically.

# Version 0.3.19 (2019-05-02)
* Fix a bug when deserializing enums containing invalid bits set.

# Version 0.3.18 (2019-03-11)
* Fix an off-by-one error causing enums with 9, 17, 33, etc variants to behave
  unexpectedly.

# Version 0.3.17 (2019-03-11)
* Add an `#[enumset_serialize_as_list]` attribute to cause the `Deserialize`
  and `Serialize` implementations for `EnumSet<T>` to serialize it as a seq
  of `T`s rather than an integer bitmask.

# Version 0.3.16 (2019-02-04)
* Added `Default` implementation for `EnumSet<T>` that returns an empty map.

# Version 0.3.15 (2019-01-08)
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
