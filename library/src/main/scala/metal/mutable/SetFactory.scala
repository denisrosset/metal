package metal
package mutable

import spire.syntax.cfor._

trait SetFactory extends generic.SetFactory {

  type S[K] <: mutable.Set[K]

  def reservedSize[A:MetalTag:Extra](n: Int): S[A]

  def empty[A:MetalTag:Extra]: S[A] = reservedSize[A](0)

  def apply[A:MetalTag:Extra](items: A*): S[A] = fromIterable(items)

  def fromIterable[A:MetalTag:Extra](items: Iterable[A]): S[A] = {
    val set = reservedSize[A](items.size)
    items.foreach { k =>
      set.ptrAddKey(k)
    }
    set
  }

  def fromArray[A:MetalTag:Extra](array: Array[A]): S[A] = {
    val set = reservedSize[A](array.length)
    cforRange(0 until array.length) { i =>
      set.ptrAddKeyFromArray(array, i)
    }
    set
  }

}
