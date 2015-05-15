package net.alasc.ptrcoll
package sets

import scala.{specialized => sp}
import scala.reflect.ClassTag

import spire.syntax.cfor._

trait MutSSetFactory[LB, Extra[_]] {
  type LBEv[A] = A <:< LB
  def empty[@sp(Int) A: ClassTag: Extra: LBEv]: MutSSet[A]
  def apply[@sp(Int) A: ClassTag: Extra: LBEv](items: A*): MutSSet[A]
  def ofSize[@sp(Int) A: ClassTag: Extra: LBEv](n: Int): MutSSet[A]
  def fromArray[@sp(Int) A: ClassTag: Extra: LBEv](array: Array[A]): MutSSet[A] = {
    val sset = ofSize[A](array.length)
    cforRange(0 until array.length) { i =>
      sset += array(i)
    }
    sset
  }
}
