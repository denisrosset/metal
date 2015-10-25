## Metal - fast unboxed data structures for Scala

Metal provides fast mutable *containers* whose performance should be close to
hand-written data structures using raw Java arrays.

In particular:

- Metal containers are themselves not specialized, but, thank to macros, avoid
  boxing when accessing, storing and updating elements;
- Metal provides higher-order methods such as `foreach`, `count`, `exists`, ...
  that are translated into `while` loops during compilation, and whose arguments
  are inlined, to avoid allocation of closures;
- Scala iterators are replaced by pointers, represented by value classes that can
  be manipulated as primitives;
- mutable containers can be used as builders for immutable containers without
  additional allocations.

The library is inspired by [Debox](http://github.com/non/debox), and reuses
parts of its data structures (for example the data structures behind hashed sets and maps).

For performance reasons, Metal's types are neither compatible with Scala's
collections framework, nor the Debox implementations.

The set of methods available on Metal instances is limited, but guarantees
that no allocations occur outside those for the underlying storage of the collection
items.
