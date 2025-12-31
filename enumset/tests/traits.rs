#![deny(warnings)]

use enumset::EnumSet;
use enumset::EnumSetType;

#[test]
fn send_sync() {
    #![allow(dead_code)]

    fn drop_thread_safe<T: Send + Sync>(_: T) {}

    fn generic<T: EnumSetType>(set: EnumSet<T>) {
        drop_thread_safe(set);
    }
}
