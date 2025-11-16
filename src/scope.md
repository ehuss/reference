# Scope

## Introduction

The scope of the Rust Reference is to fully describe the syntax and semantics of the Rust language. In this context, the language encompasses:

- The valid syntax and forms.
- The meaning and interpretation of the syntax.
- The semantic properties of the runtime behavior.
- The built-in attributes.
- The built-in types (except for the standard library implementations, see below).
- The standard library only at the boundaries where the aforementioned parts directly interface with the standard library. An item at these boundaries is sometimes called a *language item*.
- All [editions] of Rust.

## Out of scope

The Reference does not include the following:

- The contents of the standard library unless required to describe other language aspects.
- It is not intended as an introduction or guide to the language. Background familiarity with the language is assumed. A separate [book] is available to help acquire such background familiarity.
- The specifics of the tooling provided by the Rust organization, such as `rustc` or Cargo.
    - `rustc` has its own [book][rustc book]. Cargo has a [book][cargo book] that contains a [reference][cargo reference].
- The specifics of which targets exist, or the properties of specific targets. See [target-specific behavior].
- Unstable features that are only available on the [nightly channel]. For documentation of unstable features, see the [Unstable Book].
- Allowed or disallowed optimizations, unless otherwise specifically afforded by the language.
- Version history or past behavior. Changes in Rust releases are recorded in the [Release Notes]. Prior releases of the Reference are available as described in [Rust releases].
- `rustc` lints, unless they have specific relation to some part of the language. Lints are documented in the [rustc book][lints].
- The rationale for most aspects of the language. These may be included in some situations, particularly when it may not be obvious or has particularly useful relevance. But in most cases, the rationale would be far too large, and thus is out of scope for this document.
- The method of translation into executable code unless required (for example, as it relates to the translation of [inline assembly]).
- The limits of the input to the compiler, unless specified (for example, see [limits]).
    TODO: This is worded oddly, since it doesn't explain why some limits would be described and not others. Maybe we should explicitly say limits (or lack thereof) in all situations?

## Completeness

For historical reasons, portions of the Reference may be incomplete or undecided. New features and changes must always be completely documented unless they involve sections of the language that were previously undocumented. Work is ongoing to backfill these sections with the intent that all parts will eventually be complete.

## Correctness

It intended that the Reference is correct in all aspects, but errors or ambiguities are inevitable in a prose-driven specification. In most cases when it is discovered that there is an error or ambiguity in the Reference, then the Reference is usually updated to fix that. Whether or not something is an error can sometimes be unclear, and in those situations we consult:

- The behavior in `rustc`.
- Historical documentation and communication.
- The [Language Team].

## Intended versus actual behavior

At times the actual behavior of the canonical (reference?) `rustc` implementation diverges from the documented or intended behavior. The general intention is that the Reference documents the intended behavior, even if that differs from the actual behavior in `rustc`. However, at times the intended behavior is not clear, or it seems infeasible to change `rustc` to match the intended behavior (at least without an edition change). In those cases, we tend to document the actual behavior in `rustc`, and may include an informational note to highlight this discrepancy.

Documenting all bugs and flaws in `rustc` is out of scope for the Reference. TODO

## Deprecated behavior

The Reference does not directly document behavior that is intended to be deprecated (that is, removed) in a future release except in the following ways:

- Past behavior that has been deprecated and changed or removed as part of an edition is included in [edition annotations][editions].
- In exceptional circumstances, informational notes may indicate that `rustc` does not conform to the intended behavior, and that it may change in the future. See [Intended versus actual behavior](#intended-versus-actual-behavior).

## Unspecified behavior

*Unspecified behavior* is behavior that is not explicitly defined, but still covers a well-formed program. It should be a relatively rare concept as the intent is that programmers can rely on expected behavior in as many areas as possible. TODO: Provide guidance on when things are unspecified.

## Undefined behavior

[*Undefined behavior*][undefined] is compile-time or run-time behavior that is not specified. See the corresponding chapter for a complete description. All undefined behavior should be documented in that chapter. Other chapters may also include mentions of undefined behavior where it is relevant, but should also link back to the [undefined behavior][undefined] chapter.

## Implementation-defined behavior

*Implementation-defined behavior* is behavior is a variant of [unspecified behavior](#unspecified-behavior) where an implementation is responsible for documenting its behavior. Generally the Reference does not define implementation-defined behavior aside from the following:

- [Target-specific behavior](#target-specific-behavior)

TODO: Anything else here?


## Target-specific behavior

The Reference does not document which targets exist, or the properties of specific targets. The Reference may refer to *platforms* or *target properties* where required by the language. Some examples:

* Conditional-compilation keys like [`target_os`] are specified to exist, but not what their values must be.
* The [`windows_subsystem` attribute] specifies that it only works on Windows platforms.
* [Inline assembly] and the [`target_feature` attribute] specify the architectures that are supported.

## Normative and informational content

TODO: Relation to Rules, introductions, Notes, Examples, appendices, etc.
Do we want to make explicit distinction of normative vs non-normative (informative)?

[book]: ../book/index.html
[Unstable Book]: https://doc.rust-lang.org/nightly/unstable-book/
[nightly channel]: ../book/appendix-07-nightly-rust.html
[cargo book]: ../cargo/index.html
[cargo reference]: ../cargo/reference/index.html
[rustc book]: ../rustc/index.html
[`target_os`]: cfg.target_os
[target-specific behavior]: #target-specific-behavior
[`windows_subsystem` attribute]: runtime.windows_subsystem
[Inline assembly]: asm
[`target_feature` attribute]: attributes.codegen.target_feature
[Rust releases]: introduction.md#rust-releases
[Release Notes]: https://doc.rust-lang.org/releases.html
[lints]: ../rustc/lints/index.html
[limits]: attributes.limits
[Language Team]: http://github.com/rust-lang/lang-team
[editions]: introduction.md#editions
