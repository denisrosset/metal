package metal.immutable

import spire.algebra.Order
import metal.Methods

final class ArraySortedSet[K](val items: Array[K], val longSize: Long)(implicit val K: Methods[K], val order: Order[K]) extends metal.ArraySortedSet[K] with metal.immutable.SortedSet[K]

object ArraySortedSet extends metal.immutable.SetFactory {

  type Extra[K] = Order[K]
  type S[K] = metal.immutable.ArraySortedSet[K]
  type M[K] = metal.mutable.ArraySortedSet[K]

  def mutableFactory = metal.mutable.ArraySortedSet

}
