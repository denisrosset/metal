package net.alasc.ptrcoll
package sets

import scala.{specialized => sp}
import scala.reflect.ClassTag

import spire.syntax.cfor._

trait SSetFactory[LB, Extra[_]] {
  type LBEv[A] = A <:< LB
  def empty[@sp(Int) A: ClassTag: Extra: LBEv]: SSet[A]
  def apply[@sp(Int) A: ClassTag: Extra: LBEv](items: A*): SSet[A]
  def ofSize[@sp(Int) A: ClassTag: Extra: LBEv](n: Int): SSet[A]
  def fromArray[@sp(Int) A: ClassTag: Extra: LBEv](array: Array[A]): SSet[A] = {
    val sset = ofSize[A](array.length)
    cforRange(0 until array.length) { i =>
      sset += array(i)
    }
    sset
  }
}
