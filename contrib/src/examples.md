# Examples

Code examples should use code blocks with triple backticks. The language should always be specified (such as `rust`).

```rust
println!("Hello!");
```

See <https://rust-lang.github.io/mdBook/format/theme/syntax-highlighting.html#supported-languages> for a list of supported languages.

Rust examples are tested via rustdoc, and should include the appropriate annotations:

* `edition2015`, `edition2018`, etc. --- If it is edition-specific (see `book.toml` for the default).
* `no_run` --- The example should compile successfully, but should not be executed.
* `should_panic` --- The example should compile and run, but produce a panic.
* `compile_fail` --- The example is expected to fail to compile.
* `ignore` --- The example shouldn't be built or tested. This should be avoided if possible. Usually this is only necessary when the testing framework does not support it (such as external crates or modules, or a proc-macro), or it contains pseudo-code which is not valid Rust. An HTML comment such as `<!-- ignore: requires extern crate -->` should be placed before the example to explain why it is ignored.
* `Exxxx` --- If the example is expected to fail to compile with a specific error code, include that code so that rustdoc will check that the expected code is used.

When demonstrating success cases, many such cases may be included in a single code block. For failure cases, however, each example must appear in a separate code block so that the tests can ensure that each case indeed fails and fails with the appropriate error code or codes.

See the [rustdoc documentation] for more detail.

[rustdoc documentation]: https://doc.rust-lang.org/rustdoc/documentation-tests.html

You can verify the samples pass by running `mdbook test`.
