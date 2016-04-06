package metal
package mutable

import scala.reflect.ClassTag

import spire.syntax.cfor._

trait SetFactory extends generic.SetFactory {

  type S[K] <: mutable.Set[K]

  def reservedSize[A:ClassTag:Extra](n: Long): S[A]

  def empty[A:ClassTag:Extra]: S[A] = reservedSize[A](0)

  def apply[A:ClassTag:Extra](items: A*): S[A] = fromIterable(items)

  def fromIterable[A:ClassTag:Extra](items: Iterable[A]): S[A] = {
    val set = reservedSize[A](items.size)
    items.foreach { k =>
      set.ptrAddKey(k)
    }
    set
  }

  def fromArray[A:ClassTag:Extra](array: Array[A]): S[A] = {
    val set = reservedSize[A](array.length)
    cforRange(0 until array.length) { i =>
      set.ptrAddKeyFromArray(array, i)
    }
    set
  }

}
