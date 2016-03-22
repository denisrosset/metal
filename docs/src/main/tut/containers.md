---
layout: default
title:  "Container types"
section: "tutorials"
---

# Container types

All container names are prefixed by one of:

- `M`: the mutable variant,
- `I`: the immutable variant,
- `F`: the generic variant, that can be either mutable or immutable.

The following set implementations are provided (where `x = M/I/F`):

- `xHashSet[K]`: a set implemented using an open addressing scheme,
- `xSortedSet[K]`: a sorted set, using the `Order` type class from `Spire`,
- `xBitSet[Int]` a bitset implementation with (non-negative) `Int` keys.

Two variants of maps are implemented:

- `xHashMap[K,V]`: an hash map using an open addressing scheme,
- `xHashMap2[K,V1,V2]`: an hash map usin an open addressing scheme, storing
  values `(V1, V2)`; however, the values of type `V1` and `V2` are never
  stored as a tuple to avoid allocations; individual access methods are
  provided instead.

There is also a work-in-progress implementation of mutable and growable arrays
`Buffer` and an immutable wrapper for arrays `IArraySeq`.
