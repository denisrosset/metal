package metal

import spire.algebra.Order

abstract class SortedSet[K] extends metal.Set[K] {

  implicit def order: Order[K]

  type Immutable <: metal.immutable.SortedSet[K]
  type Mutable <: metal.mutable.SortedSet[K]

}
