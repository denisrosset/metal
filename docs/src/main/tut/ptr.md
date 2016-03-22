---
layout: default
title:  "Pointers"
section: "tutorials"
source: "core/src/main/scala/metal/Ptr.scala"
scaladoc: "#metal.Ptr"
---

# Pointers

Metal defines two pointer types, `Ptr` and `VPtr`. The `Ptr` pointer can either point
to an element in a container, or be a null pointer. The `VPtr` pointer is guaranteed to point
to an element.

Pointers are invalidated when the container is modified, with the exception of the 
`removeAndAdvance` methods.

A pointer has a single type parameters, representing the singleton type of the pointed collection. Pointers
are implemented using value classes containing a primitive `Long`.

Most of the container methods return possibly null `Ptr` instances. To convert a `Ptr`
to a `VPtr`, the following syntax is encouraged:

```tut
import metal._
val container = mutable.HashSet(1, 2, 3)
val p = container.ptr
p match {
  case IsVPtr(vp) =>
    // now `vp` is a `VPtr`
    println(vp.key)
  case _ =>
    println("container is empty")
}
```

Thanks to name-based extractors, the code snippet above does not perform any allocations.

Several methods are implemented on `Ptr` and `VPtr`, and can be used to access the pointed
element.

```tut
import metal._
import metal.syntax._

val m = mutable.HashMap(1 -> 2, 3 -> 4)
// we request a pointer to the first element in the map (hash maps have an internal arbitrary order)
val p = m.ptr
assert(!p.isNull)
assert(p.nonNull)
assert(p.keyOrElse(-1) > 0)
// does not throw, `sys.error` is inlined by macros and only called when `p` is null
p.keyOrElse(sys.error("")) > 0
// same syntax for pointed values
p.valueOrElse(-1)
p match {
  case IsVPtr(vp) =>
    // vp is now a non-null `VPtr`, the pointed key is available
    assert(vp.key > 0)
    assert(vp.value % 2 == 0)
    // we can ask for a (possibly null) pointer to the next element, according
    // to the hash map internal order
    val nxt = vp.next
  case _ =>
}
// we can look for keys
val p1 = m.ptrFind(3)
// and the same methods are available on that pointer
assert(p1.nonNull)
```
