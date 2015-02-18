## PtrColl - Pointer collections

PtrColl provides fast, specialized mutable collections that never boxe neither
allocate helper objects.

The library is heavily inspired by [Debox](http://github.com/non/debox), and uses
parts of its code (hashed sets and maps).

For performance reasons, PtrColl's types are neither compatible with Scala's
collections framework, nor the Debox implementations.

The set of methods available on PtrColl instances is very limited, but guarantees
that no allocations occur outside those for the underlying storage of the collection
items.

PtrColl replaces collection iterators by collection pointers; under the hood, these
pointers are represented by primitive values (scala Long) whose interpretation
depend on the particular collection they are attached to.

Given a collection `coll` and a pointer `ptr: coll.Ptr`, three operations
can be called:

- `ptr.hasAt: Boolean` checks if the pointer points to an object in the
underlying collection, or if the pointer is outside the collection bounds,
- `ptr.at: A` retrieves the value pointed,
- `ptr.next: Ptr` returns a pointer to the next element in the collection.

PtrColl has an implicit syntax mechanism borrowed from
[Spire](http://github.com/non/spire) and automatically calls the corresponding
methods on the collection.

A simple code example is given by:

```scala
import net.alasc.ptrcoll

import ptrcoll.syntax.all._
import ptrcoll.sets._

// collection instance (standard syntax)
val set = HashSSet("Hi", "Ho", "Ha")

// import of the collection pointer typeclass
import set.PtrTC

// initializes a pointer to the first element
var ptr = set.pointer

// iterates through the set
while (ptr.hasAt) {
  println(ptr.at)
  ptr = ptr.next
}
```

No allocation is ever performed after the initialization of `set`.

### Collection types

Only sets (named `SSet`, doubling the initial letter) are implemented
for now in three variants:

- `BitSSet` is a variant of `scala.collection.mutable.BitSet`, where
the set of integers is encoded as a bitstring in a `Array[Long]`,
- `SortedSSet` stores the elements in a sorted array, and checks
  membership using a binary search,
- `HashSSet` uses a hash table implementation coming from Debox.

Additional types such as `MMap` and `BBuffer` are on the roadmap.
