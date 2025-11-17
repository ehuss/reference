# Scope

## Introduction

The scope of the Rust Reference is to fully describe the syntax and semantics of the Rust language. In this context, the language encompasses:

- The valid syntax and forms.
- The meaning and interpretation of the syntax.
- The semantic properties of the runtime behavior.
- The built-in attributes.
- The built-in types (except for the standard library implementations, see below).
- The standard library, but only at the boundaries where it directly interfaces with the language. An item at these boundaries is sometimes called a *language item*.
- All [editions] of Rust.

## Out of scope

The Reference does not include the following:

- The contents of the standard library unless required to describe other language aspects.
    - The standard library is described in [the standard library API documentation][std].
- It is not intended to be an introduction or guide to the language. Background familiarity with the language is assumed. A separate [book] is available to help acquire such background familiarity.
- The specifics of the tooling provided by the Rust organization, such as `rustc` or Cargo.
    - `rustc` has its own [book][rustc book]. Cargo has a [book][cargo book] that contains a [reference][cargo reference].
- The specifics of which targets exist, or the properties of specific targets. See [target-specific behavior] for more details.
- Unstable features that are only available on the [nightly channel].
    - For documentation of unstable features, see the [Unstable Book].
- Version history or past behavior.
    - Changes in Rust releases are recorded in the [Release Notes]. Prior releases of the Reference are available as described in [Rust releases].
- `rustc` lints, unless they have specific relation to some part of the language.
    - Lints are documented in the [rustc book][lints].
- The rationale for most aspects of the language. Rationale may be included in some situations, particularly when a feature is not obvious or has especially useful relevance. But in most cases, the rationale would be far too large, and thus is out of scope for this document.
- The method of translation into executable code unless required (for example, as it relates to the translation of [inline assembly]).
- Allowed or disallowed optimizations, unless otherwise specifically afforded by the language.
- Limits on compiler inputs (such as maximum source file size) unless the language explicitly defines specific limits (for example, see [limits] or the [number of `#` symbols in raw strings][lex.token.literal.str-raw.intro]).

## Completeness

For historical reasons, portions of the Reference may be incomplete or undecided. New features and changes must always be completely documented unless they involve sections of the language that were previously undocumented. Work is ongoing to backfill these incomplete sections with the intent that all sections will eventually be complete.

## Correctness

The Reference is intended to be correct in all aspects, but errors or ambiguities are inevitable in a prose-driven specification. In most cases, when an error or ambiguity is discovered, the Reference is updated to correct it. Whether or not something is an error can sometimes be unclear, and in those situations the Reference editors consult:

- The behavior in `rustc`.
- Historical documentation and communication.
- The [Language Team].

## Intended versus actual behavior

At times the actual behavior of the official `rustc` implementation diverges from the documented or intended behavior. The general intention is that the Reference documents the intended behavior, even if that differs from the actual behavior in `rustc`. Informational notes should be included to highlight significant discrepancies. At times the intended behavior is not clear. In those cases, we tend to document the actual behavior in `rustc`.

However, documenting every bug or implementation quirk in `rustc` is out of scope for the Reference. Only discrepancies that are significant, long-standing, or particularly likely to affect users warrant mention. Minor bugs, temporary implementation issues, or behaviors expected to be fixed in an upcoming release are typically not documented.

## Deprecated behavior

The Reference does not directly document behavior that is intended to be deprecated (that is, removed) in a future release except in the following ways:

- Past behavior that has been deprecated and changed or removed as part of an edition is included in [edition annotations][editions].
- In exceptional circumstances, informational notes may indicate that `rustc` does not conform to the intended behavior, and that it may change in the future. See [Intended versus actual behavior](#intended-versus-actual-behavior).

## Unspecified behavior

*Unspecified behavior* is behavior that is not explicitly defined, but still covers a well-formed program. It should be a relatively rare concept as the intent is that programmers can rely on expected behavior in as many areas as possible.

Behavior is typically left unspecified when:

- The language intentionally allows implementation flexibility for optimization or platform-specific concerns, but the variation does not affect program correctness (for example, the exact layout of certain types using [the `Rust` representation]).
- Multiple reasonable behaviors exist and the language team has not yet decided on a single required behavior (see [Completeness](#completeness)).
- The behavior depends on details that are deliberately abstracted away (for example, the specific values of raw pointer addresses beyond equality comparisons).

Unspecified behavior differs from [undefined behavior](#undefined-behavior) in that all possible outcomes are valid and do not compromise program safety. Programs should not rely on specific unspecified behaviors as they may vary between compiler versions, optimization levels, or platforms.

## Undefined behavior

[*Undefined behavior*][undefined] is compile-time or run-time behavior that is not specified. See the corresponding chapter for a complete description. Other chapters may also include mentions of undefined behavior where it is relevant, but should also link back to the [undefined behavior][undefined] chapter.

## Implementation-defined behavior

*Implementation-defined behavior* is a variant of [unspecified behavior](#unspecified-behavior) where an implementation is responsible for documenting its behavior. Generally the Reference does not include implementation-defined behavior aside from the following:

- [Target-specific behavior](#target-specific-behavior).
- The specific panic runtime behavior (such as the format and content of panic messages, or the exact unwinding implementation) beyond the guarantees specified in the [panic] chapter.
- The specific implementation of name mangling for symbols (beyond the stable interfaces like `#[no_mangle]`).

## Target-specific behavior

The Reference does not document which targets exist, or the properties of specific targets. The Reference may refer to *platforms* or *target properties* where required by the language. Some examples:

- Conditional-compilation keys like [`target_os`] are specified to exist, but not what their values must be.
- The [`windows_subsystem` attribute] specifies that it only works on Windows platforms.
- [Inline assembly] and the [`target_feature` attribute] specify the architectures that are supported.

## Normative and informational content

The Reference contains both normative and informational content. *Normative content* defines the official requirements and specifications of the Rust language: the rules that determine what constitutes valid Rust code and its behavior. *Informational content* provides context, examples, and clarifications that aid understanding but do not define requirements.

The normative content consists of the [rules], [grammar productions], and anything else that is explicitly listed as normative.

The informational content consists of the [notes], [examples], [warnings], introductions (rules ending in `.intro`), footnotes, and appendices (unless otherwise noted).

[`target_feature` attribute]: attributes.codegen.target_feature
[`target_os`]: cfg.target_os
[`windows_subsystem` attribute]: runtime.windows_subsystem
[book]: ../book/index.html
[cargo book]: ../cargo/index.html
[cargo reference]: ../cargo/reference/index.html
[editions]: introduction.md#editions
[examples]: introduction.md#examples
[Glossary]: glossary.md
[grammar productions]: notation.md
[Inline assembly]: asm
[Language Team]: http://github.com/rust-lang/lang-team
[limits]: attributes.limits
[lints]: ../rustc/lints/index.html
[nightly channel]: ../book/appendix-07-nightly-rust.html
[notes]: introduction.md#notes
[Release Notes]: https://doc.rust-lang.org/releases.html
[rules]: introduction.md#rules
[Rust releases]: introduction.md#rust-releases
[rustc book]: ../rustc/index.html
[std]: ../std/index.html
[target-specific behavior]: #target-specific-behavior
[the `Rust` representation]: layout.repr.rust
[Unstable Book]: https://doc.rust-lang.org/nightly/unstable-book/
[warnings]: introduction.md#warnings
