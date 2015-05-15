package net.alasc.ptrcoll
package sets

import scala.{specialized => sp}
import scala.reflect.ClassTag

import spire.algebra.Order

import syntax.all._

trait SSet[@sp(Int) K] extends Keyed[K] { lhs =>
  implicit def ctK: ClassTag[K]

  def copy: SSet[K]

  override def toString: String = {
    val sb = new StringBuilder
    sb.append("SSet(")
    var prefix = ""
    var p = pointer
    while(p.hasAt) {
      sb.append(prefix)
      sb.append(p.at.toString)
      prefix = ", "
      p = p.nextPtr
    }
    sb.append(")")
    sb.toString
  }

  /**
    * Check if two SSets are equal.
    *
    * Equal means the sets have the same type (which is checked
    * using the ClassTag instances) and the same contents.
    *
    * Comparing SSets with any of Scala's collection types will
    * return false.
    */
  override def equals(rhs: Any): Boolean = rhs match {
    case rhs: SSet[_] =>
      if (size != rhs.size || ctK != rhs.ctK) return false
      val s = rhs.asInstanceOf[SSet[K]]
      var p = lhs.pointer
      while (p.hasAt) {
        if (!s.contains(p.at)) return false
        p = p.nextPtr
      }
      true
    case _ => false
  }

  /**
    * Hashes the contents of the set to an Int value.
    *
    * By xor'ing all the set's values together, we can be sure that
    * sets with the same contents will have the same hashCode
    * regardless of the order those elements appear.
    *
    * This is an O(n) operation.
    */
  override def hashCode: Int = {
    var hash: Int = 0xDEADD065
    var p = lhs.pointer
    while (p.hasAt) {
      hash ^= p.at.##
      p = p.nextPtr
    }
    hash
  }
}

trait MutSSet[@sp(Int) K] extends SSet[K] with MutKeyed[K] { lhs =>
  /**
    * Adds item to the set.
    * 
    * Returns whether or not the item was added. If item was already in
    * the set, this method will do nothing and return false.
    */
  def add(item: K): Boolean

  /** Adds item to the set, and returns the set. */
  def +=(item: K): lhs.type

  def copy: MutSSet[K]
}
