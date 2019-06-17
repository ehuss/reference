# Name resolution

Elements, such as some [item definitions] or [`let` bindings], declare a name
to refer to that element. _Name resolution_ is the process of tying paths and
other identifiers to the declarations of those elements. Names are segregated
into different [namespaces], allowing elements in different namespaces to
share the same name without conflict. Each name is valid within a [scope], or
a region of source text where that name may be referenced. Access to certain
names may be restricted based on their [visibility].

[`let` bindings]: ../statements.md#let-statements
[item definitions]: ../items.md
[namespaces]: namespaces.md
[scope]: scopes.md
[visibility]: ../visibility-and-privacy.md
