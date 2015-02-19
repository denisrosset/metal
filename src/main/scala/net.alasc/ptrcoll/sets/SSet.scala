package net.alasc.ptrcoll
package sets

import scala.{specialized => sp}
import scala.reflect.ClassTag

import spire.algebra.Order

import syntax.all._

trait Dummy[@sp(Int) A]

object Dummy {
  implicit def fakeInstance[@sp(Int) A]: Dummy[A] = null
}

trait SSetFactory[LB, Extra[_]] {
  type LBEv[A] = A <:< LB
  def empty[@sp(Int) A: ClassTag: Extra: LBEv]: SSet[A]
  def apply[@sp(Int) A: ClassTag: Extra: LBEv](items: A*): SSet[A]
  def ofSize[@sp(Int) A: ClassTag: Extra: LBEv](n: Int): SSet[A]
}

trait SSet[@specialized(Int) A] extends PointableAt[A] with Findable[A] with Sized { self =>
  implicit def ct: ClassTag[A]

  /**
    * Return whether the item is found in the SSet or not.
    * 
    * Performance depends on the underlying implementation.
    */
  def apply(item: A): Boolean

  /**
    * Adds item to the set.
    * 
    * Returns whether or not the item was added. If item was already in
    * the set, this method will do nothing and return false.
    */
  def add(item: A): Boolean

  /**
    * Remove an item from the set.
    * 
    * Returns whether the item was originally in the set or not.
    */
  def remove(item: A): Boolean

  /** Removes the element pointed by `ptr`, and returns
    * the next pointer in the iteration. */
  def removeAt(ptr: Ptr): Ptr

  /** Adds item to the set. Calls `add`. */
  def +=(item: A): self.type = { add(item); self }

  /** Removes item from set. Calls `remove`. */
  def -=(item: A): self.type = { remove(item); self }

  def --=(pt: PointableAt[A]): self.type = {
    import pt.{PtrTC => ptPtrTC}
    var ptr = pt.pointer
    while (ptr.hasAt) {
      self.remove(ptr.at)
      ptr = ptr.next
    }
    self
  }

  override def toString: String = {
    val sb = new StringBuilder
    var prefix = "SSet("
    var p = pointer
    while(p.hasAt) {
      sb.append(prefix)
      sb.append(p.at.toString)
      prefix = ", "
      p = p.next
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
  override def equals(that: Any): Boolean = that match {
    case that: SSet[_] =>
      if (size != that.size || ct != that.ct) return false
      val rhs = that.asInstanceOf[SSet[A]]
      var p = self.pointer
      while (p.hasAt) {
        if (!rhs(p.at)) return false
        p = p.next
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
    var p = self.pointer
    while (p.hasAt) {
      hash ^= p.at.##
      p = p.next
    }
    hash
  }
}
