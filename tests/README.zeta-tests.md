Zeta Integration Tests Harness

- Testing library/framework: Rust built-in test harness (#[test], assert_eq\!), no external deps.
- These tests expect an executable capable of reading Zeta source code from stdin and printing program output to stdout.
- Set environment variable ZETA_RUNNER to the path of that executable before running `cargo test`, for example:
    ZETA_RUNNER=./target/debug/zeta-runner cargo test
- If ZETA_RUNNER is not set, tests will fail early with a clear error message.