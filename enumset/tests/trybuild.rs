#[test]
fn ui() {
    if version_check::is_min_version("1.36.0") {
        let t = trybuild::TestCases::new();
        t.compile_fail("tests/compile-fail/*.rs");
    }
}
