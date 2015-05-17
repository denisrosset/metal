package net.alasc.ptrcoll
package sets

import scala.{specialized => sp}
import scala.reflect.ClassTag
import scala.annotation.tailrec

import spire.algebra.Order

trait SSet[@sp(Int) K] extends Countable with Searchable[K] { lhs =>
  implicit def ctK: ClassTag[K]

  def copy: SSet[K]

  override def toString: String = {
    val sb = new StringBuilder
    sb.append("SSet(")
    @tailrec def rec(p: Ptr, prefix: String): Unit = p match {
      case Valid(vp) =>
        sb.append(prefix)
        sb.append(ptrKey(vp).toString)
        rec(ptrNext(vp), ", ")
      case _ =>
    }
    rec(ptrStart, "")
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
    case rhs: SSet[K] if size == rhs.size && ctK == rhs.ctK =>
      @tailrec def rec(lp: Ptr): Boolean = lp match {
        case Valid(vlp) =>
          if (rhs.contains(ptrKey(vlp)))
            rec(ptrNext(vlp))
          else
            false
        case _ => true
      }
      rec(lhs.ptrStart)
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
    @tailrec def rec(p: Ptr, h: Int): Int = p match {
      case Valid(vp) => rec(ptrNext(vp), h ^ ptrKey(vp).##)
      case _ => h
    }
    rec(ptrStart, 0xDEADD065)
  }
}

trait MutSSet[@sp(Int) K] extends SSet[K] with Removable[K] { lhs =>
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
