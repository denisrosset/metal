## Metal - fast unboxed data structures for Scala

See the [companion website](https://denisrosset.github.io/metal) and the
associated [tutorials](https://denisrosset.github.io/metal/tutorials.html)
Metal provides fast mutable collections whose performance should be close to
hand-written data structures using raw primitive arrays.

In particular:

- Metal collections are themselves not specialized, but will avoid any boxing/allocation
  when accessing, storing and updating elements, thanks to macros and specialized
  *methods*;
- Metal provides higher-order methods such as `foreach`, `count`, `exists`, ...
  that are translated into `while` loops during compilation. The loop body
  is inlined, avoiding allocation of closures;
- Scala iterators are replaced by pointers, represented by value classes that
  need no allocation;
- mutable containers can be used as builders for immutable containers.

The library is heavily inspired by [Debox](http://github.com/non/debox). Parts of the
implementation are similar (for example the strategy for hash sets and maps).

For performance reasons, Metal's types are neither compatible with Scala's
collections framework, nor the Debox implementations; but the methods in
Metal's interfaces are either prefixed by `ptr`, or follow currently used
conventions (e.g. `def isEmpty: Boolean`).

Higher-order methods such as `foreach` are provided by enrichment methods.

The set of methods available on Metal instances is limited, but guarantees
that no allocations occur except when creating or growing containers.

## Structure of Metal

Metal is composed of two packages:

- `core` contains the definition of pointer types (see below), traits that are
  inherited by containers to provide capabilities, and macro implementations of
  the standard collection methods;
- `library` provides implementations of standard data structures, with
  `generic` providing base types such as `Set`, `Map`, `Map2`, `Buffer`,
  mutable variants in `mutable` with corresponding immutable variants
  in `immutable`.
  
We currently have a dependency on Spire for two reasons:

- availability of a non-boxing option type `spire.util.Opt`,
- compatibility macro shims for Scala 2.10 and 2.11.

## Example

```scala
scala> import metal._; import syntax._
import metal._
import syntax._

scala> val set = mutable.HashSet(1,2,3,4)
set: metal.mutable.HashSet.S[Int] = Set(1, 2, 3, 4)

scala> set.contains(3)
res0: Boolean = true

scala> set.foreach { k => println(k) }
1
2
3
4


scala> set.result()
res2: metal.immutable.HashSet[Int] = Set(1, 2, 3, 4)

scala> set
res3: metal.mutable.HashSet.S[Int] = Set()

scala> val map = mutable.HashMap(1 -> "test")
map: metal.mutable.HashMap.M[Int,String] = Map(1 -> test)

scala> map.getOrElse(1, sys.error(""))
res4: String = test

```
