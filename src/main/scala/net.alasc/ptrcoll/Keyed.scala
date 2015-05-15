package net.alasc.ptrcoll

import scala.{specialized => sp}
import scala.reflect.ClassTag

trait Keyed[@sp(Int, Long) K] extends Pointable[K] { lhs =>
  /** Returns a pointer to the given item, if it exists. */
  def findPointerAt(item: K): Ptr

  /** Returns whether the item is present in the collection. */
  def contains(item: K): Boolean
}

trait MutKeyed[@sp(Int, Long) K] extends Keyed[K] { lhs =>
  /** Removes any value associated with key, and returns whether
    * an operation was performed.
    */
  def remove(key: K): Boolean

  /** Removes the pointed element. */
  def removeAt(ptr: ValidPtr): Unit

  /** Removes the pointed element, and returns the pointer
    * to the next element.
    */
  def removeAndAdvance(ptr: ValidPtr): Ptr

  /** Removes item from collection. Calls `remove`. */
  def -=(item: K): lhs.type

  def --=(pt: Pointable[K]): lhs.type = {
    import syntax.all._
    import pt.{PtrTC => ptPtrTC}
    var ptr = pt.pointer
    while (ptr.hasAt) {
      lhs.remove(ptr.at)
      ptr = ptr.nextPtr
    }
    lhs
  }
}
