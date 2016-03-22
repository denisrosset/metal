package metal
package mutable

import spire.syntax.cfor._
import generic.Methods

trait SetFactory extends generic.SetFactory {

  type S[K] <: mutable.Set[K]

  def reservedSize[A:Methods:Extra](n: Int): S[A]

  def empty[A:Methods:Extra]: S[A] = reservedSize[A](0)

  def apply[A:Methods:Extra](items: A*): S[A] = fromIterable(items)

  def fromIterable[A:Methods:Extra](items: Iterable[A]): S[A] = {
    val set = reservedSize[A](items.size)
    items.foreach { k =>
      set.ptrAddKey(k)
    }
    set
  }

  def fromArray[A:Methods:Extra](array: Array[A]): S[A] = {
    val set = reservedSize[A](array.length)
    cforRange(0 until array.length) { i =>
      set.ptrAddKeyFromArray(array, i)
    }
    set
  }

}
