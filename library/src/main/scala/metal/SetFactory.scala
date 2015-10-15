package metal

import scala.reflect.ClassTag

import spire.syntax.cfor._

import syntax._

trait SetFactory[LB, Extra[_]] {

  type LBEv[A] = A <:< LB

  def empty[A:ClassTag:Extra:LBEv]: Set[A]

  def apply[A:ClassTag:Extra:LBEv](items: A*): Set[A]

  def ofSize[A:ClassTag:Extra:LBEv](n: Int): Set[A]

  def fromArray[A:ClassTag:Extra:LBEv](array: Array[A]): Set[A] = {
    val sset = ofSize[A](array.length)
    cforRange(0 until array.length) { i =>
      sset += array(i)
    }
    sset
  }

}
