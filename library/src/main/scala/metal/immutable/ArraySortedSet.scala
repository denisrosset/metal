package metal
package immutable

import scala.reflect.ClassTag

import spire.algebra.Order

final class ArraySortedSet[K](
  private[metal] val items: Array[K],
  val longSize: Long)(implicit
    val ctK: ClassTag[K],
    val K: MetalTag[K],
    val order: Order[K]
) extends generic.ArraySortedSet[K] with immutable.SortedSet[K] {

  type Scala = scala.collection.immutable.SortedSet[K]

  def toScala = new WrappedSortedSet[K, metal.immutable.ArraySortedSet[K], scala.collection.immutable.SortedSet[K]](this) {

    def myBuilder = scala.collection.immutable.SortedSet.newBuilder[K]

  }

}

object ArraySortedSet extends immutable.SetFactory {

  type Extra[K] = Order[K]
  type S[K] = immutable.ArraySortedSet[K]
  type M[K] = mutable.ArraySortedSet[K]

  def mutableFactory = mutable.ArraySortedSet

}
