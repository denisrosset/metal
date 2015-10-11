package net.alasc.ptrcoll
package seqs

import scala.reflect.ClassTag
import scala.{specialized => sp}

import spire.algebra._
import spire.math.QuickSort
import spire.syntax.all._

trait Seq[@sp(Int, Long) V] extends Searchable[Int] with Countable with PointableValue[V] { lhs =>

  implicit def ct: ClassTag[V]

  override def equals(that: Any): Boolean = that match {
    case s: Seq[_] =>
      if (length != s.length || ct != s.ct) return false
      val seq = s.asInstanceOf[Seq[V]]
      val limit = length
      cfor(0)(_ < limit, _ + 1) { i =>
        if (apply(i) != seq.apply(i)) return false
      }
      true
    case _ =>
      false
  }

  /**
    * Hash the contents of the buffer to an Int value.
    */
  override def hashCode: Int = {
    var code: Int = 0xf457f00d
    val limit = length
    cfor(0)(_ < limit, _ + 1) { i => code = (code * 19) + apply(i).## }
    code
  }

  /**
    * Return a string representation of the contents of the buffer.
    */
  override def toString =
    if (length == 0) {
      "Seq()"
    } else {
      val limit = length
      val sb = new StringBuilder()
      sb.append("Seq(")
      sb.append(apply(0))
      cfor(1)(_ < limit, _ + 1) { i =>
        sb.append(", ")
        sb.append(apply(i))
      }
      sb.append(")")
      sb.toString
    }

  def copy: Seq[V]

  /**
    * Return the value at element i.
    * 
    * If the index exceeds the length, the result is undefined; an exception could be
    * thrown, but this is not guaranteed.
    */
  def apply(i: Int): V

  def length: Int

  def size: Int = length

}

trait MSeq[@sp(Int, Long) V] extends Seq[V] with Removable[Int] { lhs =>

  def copy: MSeq[V]

  /**
    * Update the value of element i.
    * 
    * This method has similar caveats to apply. If an illegal i value
    * is used, the opreation is undefined.
    */
  def update(i: Int, value: V): Unit

  /** Adds item to the seq, and returns the seq. */
  def +=(item: V): lhs.type

}