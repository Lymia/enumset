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
