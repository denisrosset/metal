---
layout: default
title:  "Higher-order functions"
section: "tutorials"
---

# Higher-order functions

Several higher-order functions are available on containers, for example `foreach`, `count`,
`exists`, `forall`, `foldLeft` (or `/:`); however, the calling convention is slightly different
from the Scala collections to avoid allocating tuples:

```tut:silent
import metal._; import syntax._
val m = mutable.HashMap(1 -> 2, 3 -> 4)
```

```tut
m.foreach { (k, v) => println(s"($k, $v)") }
```

instead of

```scala
m.foreach { case (k, v) => println(s"($k, $v)") }
```

The methods `min`, `max`, `sum`, `product` are also available; the required algebraic
operations (orders, additive monoids, multiplicative monoids) are provided using
Spire type classes. Contrary to the standard Scala library, Spire is heavily
specialized on primitive types.

Those higher-order functions are implemented using implicit classes and macros, to
avoid polluting the container interfaces.

At compilation time, a call such as `m.foreach { (k, v) => println(k) }` is inlined,
producing code similar to:

```scala
@inline def rec(ptr: Ptr[m.Tag, m.Cap]): Unit = ptr match {
    case IsVPtr(vp) =>
      val k = m.ptrKey[Int](vp)
      val v = m.ptrValue[Int](vp)
      println(k)
      rec(m.ptrNext(vp))
    case _ =>
  }
```
