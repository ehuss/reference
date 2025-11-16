# Running tests

There are several different kinds of tests you can run (these are enforced on CI):

* `mdbook test`: This will run the inline Rust codeblocks (internally it uses `rustdoc` to do this).
* `cargo xtask style-check`: This will validate some style checks (see [authoring guide](docs/authoring.md)).
* `cargo xtask linkcheck`: This will validate that markdown links aren't broken.
* `cargo xtask test-all`: Runs all tests.
