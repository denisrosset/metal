package net.alasc.ptrcoll

import scala.{specialized => sp}
import scala.annotation.tailrec

trait Removable[@sp(Int, Long) K] extends PointableKeys[K] { lhs =>
  /** Removes any value associated with key, and returns whether
    * an operation was performed.
    */
  def remove(key: K): Boolean

  /** Removes the pointed element. */
  def ptrRemove(ptr: ValidPtr): Unit

  /** Removes the pointed element, and returns the pointer
    * to the next element, or null if at the end.
    */
  def ptrRemoveAndAdvance(ptr: ValidPtr): Ptr

  /** Removes key from collection. Calls `remove`. */
  def -=(key: K): lhs.type

  final def --=(coll: Nextable with Countable with PointableKeys[K]): lhs.type = {
    val c = coll
    @tailrec def rec(p: c.Ptr): Unit = p.asInstanceOf[c.Ptr] match {
      case Valid(vp) =>
        lhs.remove(c.ptrKey(vp))
        rec(c.ptrNext(vp))
      case _ =>
    }
    rec(c.ptrStart)
    lhs
  }
}
