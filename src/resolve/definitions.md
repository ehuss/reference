# Definitions

The definition of an element adds the element's name to its corresponding [namespace].
The following is a list of elements that may introduce names:


## Types
### `mod`
### `extern crate`
### `struct`
### `union`
### `enum` (and variants)
### `trait` definition
### `type` alias
### Associated type definition
### Built-in types
### Type parameters
### `Self` type
### Tool attribute modules
extern crates will shadow tool modules.
That is, a `clippy` crate will cause #[clippy::foo] to stop working (or only allow attributes from the crate).

## Values

### Function definitions
### `const` definitions
### `static` definitions
### `struct` constructors
### `enum` variant constructors
### `Self` constructors
### Method definitions
### Associated const definitions
### Local bindings
#### `let`
#### `if let`
#### `while let`
#### `for`
#### `match` arms
#### function parameters
#### closure parameters
### Captured closure variables
### Loop labels


## Macros
- Invalid (reserved) names: `cfg`, `cfg_attr`, `derive` check_reserved_macro_name
  User cannot create:
    - macro_rules!
    - function-like procedural macro
    - attribute macro
    - derive macro
        - Note: helpers can define them, but they are ignored


Invocation style influences text vs. path scope lookup:
- Single segment path (`foo!`) — first textual, then path-based if not found.
- Multi-segment path (`foo::bar!`) — path-based only

### `macro_rules!` macro
See https://github.com/rust-lang/rust/issues/35896 for tons of info.
May not be named `macro_rules`.
Un-attributed macro_rules!:
    Enters (textual) scope at point of definition.
        Valid until end of current block (module, including nested across files `mod m;`, block-expr), including nested blocks.
        May shadow:
            - Previous macro_rules! macros.
            - Built-in function-like macros.
            - function-like proc-macro
        Does not shadow (somehow magically kept separate, TODO: figure out why)
            - built-in and proc-macro attributes, derive macros (and helpers)

`#[macro_use]` on `mod`:
    Extends textual scope of macros defined in that module to extend to the end of the parent module.
        TODO: This should be clarified in macros-by-example.md.

`#[macro_use]` on `extern crate`:
    Imports all macros annotated with `macro_export` into this crate's root module.
        (into the "prelude"? TODO: need to check which prelude)
    These can be shadowed by locally defined macro_rules! macros.
    May be used before, order doesn't matter, scope is entire crate.
        "in case of a conflict, the last macro imported wins."
    Cannot be used on `extern crate self as foo;`
    Can only be done in crate root.


`#[macro_export]` on macro_rules!:
    Adds the macro to path-scope into the **crate root**.
    Order doesn't matter, may be used before defined.
    TODO: Note it is an error to export multiple macros of the same name. `duplicate_macro_exports`.
        https://github.com/rust-lang/rust/issues/35896
        Also clashes with proc-macro definitions.
    Makes it accessible from other crates (using paths).
    Can access within same crate from `crate::name` or `self::name` in root, etc.
    Interesting discussion of macro shadowing: https://github.com/rust-lang/rust/pull/50143
    TODO: Note that the order that they are added to the root is not defined? That probably doesn't matter since duplicates are not allowed.
    TODO: `use` vs exported macro of same name, probably not allowed?
    Cannot be the same name as a `#[proc_macro]` in the same crate.


### Built-in macros
Macros are defined in Resolve.builtin_macros.
    \_\_rust_unstable_column
    asm
    assert
    bench
    cfg
    column
    compile_error
    concat
    concat_idents
    env
    file
    format_args
    format_args_nl
    global_asm
    include
    include_bytes
    include_str
    line
    log_syntax
    module_path
    option_env
    stringify
    test
    test_case
    trace_macros
    Plus any plugins


### Built-in derives
Derives are defined in Resolve.builtin_macros.
    Clone
    Copy
    Debug
    Decodable
    Default
    Encodable
    Eq
    Hash
    Ord
    PartialEq
    PartialOrd
    RustcDecodable
    RustcEncodable
    Send
    Sync


### Built-in attributes
The complete list of built-in attributes are defined in BUILTIN_ATTRIBUTES.

### Tool attributes
### Function-like procedural macros
### Derive macros
### Derive macro helpers
### Attribute macros



# SCRATCH NOTES

`mod` adds the module name in the type namespace.
`mod` name scope is ….

`struct` adds the struct name to the type namespace, the name of the struct
as a constructor function to the value namespace, and the `Self` type to
the type namespace.



- Type Namespace
    - [`mod`] definitions
    - [`struct`], [`union`], [`enum`], and [`enum` variant] definitions
    - [`trait`] definitions
    - [Type aliases]
    - [Associated type] definitions
    - [Built-in types]
    - [Type parameters]
    - [`Self` type]
    - [Tool attribute modules]
- Value Namespace
    - [Function] definitions
    - [`const`] definitions
    - [`static`] definitions
    - [`struct` constructors]
    - [`enum` variant constructors]
    - [`Self` constructors]
    - [Method] definitions
    - [Associated const] definitions
    - Local bindings — [`let`], [`if let`], [`while let`], [`for`], [`match`]
      arms, [function] parameters, [closure] parameters
    - Captured [closure] variables
    - [Loop labels]
- Macro Namespace
    - [`macro_rules`] definitions
    - [Built-in macros]
    - [Built-in attributes]
    - [Tool attributes]
    - [Function-like procedural macros]
    - [Derive macros]
    - [Derive macro helpers]
    - [Attribute macros]
