package net.alasc.ptrcoll

import scala.{specialized => sp}
import scala.annotation.tailrec

trait Removable[@sp(Int, Long) K] extends PointableKey[K] { lhs =>
  /** Removes any value associated with key, and returns whether
    * an operation was performed.
    */
  def remove(key: K): Boolean

  /** Removes the pointed element. */
  def ptrRemove(ptr: VPtr[Tag]): Unit

  /** Removes the pointed element, and returns the pointer
    * to the next element, or null if at the end.
    */
  def ptrRemoveAndAdvance(ptr: VPtr[Tag]): Ptr[Tag]

  /** Removes key from collection. Calls `remove`. */
  def -=(key: K): lhs.type

  /*
  final def --=(coll: Nextable with Countable with PointableKey[K]): lhs.type = {
    @tailrec def rec(p: Ptr[coll.Tag]): Unit = p.asInstanceOf[Ptr[coll.Tag]] match {
      case VPtr(vp) =>
        lhs.remove(vp.key)
        rec(vp.next)
      case _ =>
    }
    rec(coll.ptrStart)
    lhs
  }*/
}
