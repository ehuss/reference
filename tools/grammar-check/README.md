# Reference grammar checker

This is a CLI tool for validating the Reference grammar against other tools.

## Commands

There are several different subcommands:

- `grammar-check lex-compare` — Compare tokenization between implementations.
- `grammar-check tokenize` — Convert source to tokens.
- `grammar-check tree` — Convert source to a tree.

Pass `--help` for more information.

It is recommended to run this in the release profile, especially when testing against a large corpus.

```shell
cargo r -r -- lex-compare --path /path/to/rust/tests
```

## Tools

This tool supports various parsers which are called *tools*. They are:

- `reference` — The Reference interpreter using the grammar from the Reference.
- `rustc_parse` — The AST parser from `rustc`.
- `rustc_lexer` — The low-level lexer from `rustc`.
- `proc-macro2` — The `proc-macro2` crate.
