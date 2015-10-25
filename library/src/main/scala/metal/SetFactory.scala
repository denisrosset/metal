package metal

import scala.reflect.ClassTag

import spire.syntax.cfor._

import syntax._

trait MSetFactory[LB, Extra[_], ST[_] <: MSet[_]] {

  type LBEv[A] = A <:< LB

  def empty[A:Methods:Extra:LBEv]: ST[A] = ofSize[A](0)

  def apply[A:Methods:Extra:LBEv](items: A*): ST[A] = {
    val sset = ofSize[A](items.size)
    items.foreach { k =>
      sset.ptrAddKey(k)
    }
    sset
  }

  def ofSize[A:Methods:Extra:LBEv](n: Int): ST[A]

  def fromArray[A:Methods:Extra:LBEv](array: Array[A]): ST[A] = {
    val sset = ofSize[A](array.length)
    cforRange(0 until array.length) { i =>
      sset.ptrAddKeyFromArray(array, i)
    }
    sset
  }

}
