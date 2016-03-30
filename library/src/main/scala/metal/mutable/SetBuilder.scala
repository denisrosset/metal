package metal
package mutable

import spire.syntax.cfor._

trait SetBuilder[K, SK <: mutable.Set[K]] extends generic.SetBuilder[K, SK] {

  def reservedSize(n: Int): SK

  def empty: SK = reservedSize(0)

  def apply(items: K*): SK = fromIterable(items)

  def fromIterable(items: Iterable[K]): SK = {
    val set = reservedSize(items.size)
    items.foreach { k =>
      set.ptrAddKey(k)
    }
    set
  }

  def fromArray(array: Array[K]): SK = {
    val set = reservedSize(array.length)
    cforRange(0 until array.length) { i =>
      set.ptrAddKeyFromArray(array, i)
    }
    set
  }

}
