---
layout: default
title:  "Container types"
section: "tutorials"
---

# Container types

Metal has three packages containing collections:

- `metal.generic`: a generic base trait, corresponding to an immutable or mutable instance,
- `metal.mutable`: mutable collections,
- `metal.immutable`: immutable collections.

Mutable collections can be used as builders for their immutable counterparts; the `result()` method
will provide an immutable instance, and clear the original collection.

The following set implementations are provided:

- `HashSet[K]`: a set implemented using an open addressing scheme,
- `SortedSet[K]`: a sorted set, using the `Order` type class from `Spire`,
- `BitSet` a bitset of implementation with (non-negative) `Int` keys.

Two variants of maps are implemented:

- `HashMap[K,V]`: an hash map using an open addressing scheme,
- `HashMap2[K,V1,V2]`: an hash map using an open addressing scheme, storing
  values `(V1, V2)`; however, the values of type `V1` and `V2` are not
  stored as tuples, avoiding allocations; individual access methods are
  provided.

There is also a simple array wrapper called `Buffer`, with immutable and mutable
variants; contrary to the other metal data structures, this class is specialized.
