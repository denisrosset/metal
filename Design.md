Design of metal
===============

The library has the following dependencies:

- github.com/non/spire for Opt, a low overhead Option-like type.

The library implements fast mutable collections called `containers`, and associated
operations that do not perform memory allocations (except when growing containers).

These containers do not box, but are not specialized in their type parameters. Instead,
a macro machinery replaces standard calls by specialized methods when accessing their elements.
