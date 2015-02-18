package net.alasc.ptrcoll
package sets

import scala.reflect.ClassTag

import spire.algebra.Order

import syntax.all._

trait SSet[@specialized(Int) A] extends PointableAt[A] { self =>
  implicit def ct: ClassTag[A]
  /**
    * Return the size of this SSet as an Int.
    * 
    * Since most SSets use arrays, their size is limited to what a 32-bit
    * signed integer can represent.
    * 
    * This is an O(1) operation.
    */
  def size: Int

  /**
    * Return true if the SSet is empty, false otherwise.
    * 
    * This is an O(1) operation.
    */
  def isEmpty: Boolean = size == 0

  /**
    * Return true if the Set is non-empty, false otherwise.
    * 
    * This is an O(1) operation.
    */
  def nonEmpty: Boolean = !isEmpty
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
      val rhs = that.asInstanceOf[Set[A]]
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
