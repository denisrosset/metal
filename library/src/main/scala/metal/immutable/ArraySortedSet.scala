package metal
package immutable

import spire.algebra.Order

final class ArraySortedSet[K](private[metal] val items: Array[K], val longSize: Long)(implicit val K: MetalTag[K], val order: Order[K]) extends generic.ArraySortedSet[K] with immutable.SortedSet[K]

object ArraySortedSet extends immutable.SetFactory {

  type Extra[K] = Order[K]
  type S[K] = immutable.ArraySortedSet[K]
  type M[K] = mutable.ArraySortedSet[K]

  def mutableFactory = mutable.ArraySortedSet

}
