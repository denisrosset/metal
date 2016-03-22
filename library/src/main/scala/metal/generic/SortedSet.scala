package metal
package generic

import spire.algebra.Order

abstract class SortedSet[K] extends generic.Set[K] {

  implicit def order: Order[K]

  type Immutable <: immutable.SortedSet[K]
  type Mutable <: mutable.SortedSet[K]

}
