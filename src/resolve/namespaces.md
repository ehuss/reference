# Namespaces

Element names are segregated into separate _namespaces_. The usage of a name
will look for the declaration of that name in different namespaces, based on
the context. The following is a list of namespaces, with their corresponding
elements:

- Type Namespace
    - [`mod`] definitions
    - [`extern crate`] definitions
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



# TODO
- Add lifetimes? Or does "type parameters" cover it?
- Should Lifetimes be in a separate namespace?
- For each *use*, document which NS(es) are consulted.
- How should implementations be documented.
    <S<i32>>::f() path to an impl item.
    Only in qualified paths?
    But otherwise the impl block cannot be referenced AFAIK.
- Explain `Tool attribute modules` in more detail.
