# Unsafety

Unsafe operations are those that can potentially violate the memory-safety
guarantees of Rust's static semantics.

The following language level features cannot be used in the safe subset of
Rust:

- Dereferencing a [raw pointer].
- Reading or writing a [mutable] or [external] static variable.
- Accessing a field of a [`union`], other than to assign to it.
- Calling an unsafe function (including an intrinsic or foreign function).
- Implementing an [unsafe trait].
- Declaring an [`extern`] block[^extern-2024].
- Applying an [unsafe attribute] to an item.

[^extern-2024]: Prior to the 2024 edition, extern blocks were allowed to be declared without `unsafe`.

[`extern`]: items/external-blocks.md
[`union`]: items/unions.md
[mutable]: items/static-items.md#mutable-statics
[external]: items/external-blocks.md
[raw pointer]: types/pointer.md
[unsafe trait]: items/traits.md#unsafe-traits
[unsafe attribute]: attributes.md
