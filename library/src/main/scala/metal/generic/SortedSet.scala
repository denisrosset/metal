package metal
package generic

import spire.algebra.Order

/** Base class for sorted sets, with the additional requirements:
  * 
  * - elements are enumerated using the Order given during construction,
  * - pointers to elements are ordered accordingly (i.e. ptr1.key < ptr2.key <=> ptr1 < ptr2).
  */
abstract class SortedSet[K] extends generic.Set[K] {

  implicit def order: Order[K]

  type Immutable <: immutable.SortedSet[K]
  type Mutable <: mutable.SortedSet[K]
  type Scala <: scala.collection.immutable.SortedSet[K]

  /** Returns a pointer to the smallest element greatest or equal to `item` if it exists. */
  def findOrNextAfter[@specialized L](item: L): Ptr[this.type]

  /** Returns a pointer to the largest element smallest or equal to `item` if it exists. */
  def findOrNextBefore[@specialized L](item: L): Ptr[this.type]

}
