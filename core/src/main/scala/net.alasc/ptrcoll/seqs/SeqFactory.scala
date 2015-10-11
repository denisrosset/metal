package net.alasc.ptrcoll
package seqs

import scala.{specialized => sp}
import scala.reflect.ClassTag

import spire.syntax.cfor._

trait MSeqFactory[LB, Extra[_]] {

  type LBEv[A] = A <:< LB

  def empty[@sp(Int, Long) A: ClassTag: Extra: LBEv]: MSeq[A]

  def apply[@sp(Int, Long) A: ClassTag: Extra: LBEv](items: A*): MSeq[A]

  def ofSize[@sp(Int, Long) A: ClassTag: Extra: LBEv](n: Int): MSeq[A]

  def fromArray[@sp(Int, Long) A: ClassTag: Extra: LBEv](array: Array[A]): MSeq[A] = {
    val seq = ofSize[A](array.length)
    cforRange(0 until array.length) { i =>
      seq += array(i)
    }
    seq
  }

}
