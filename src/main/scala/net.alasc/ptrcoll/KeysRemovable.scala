package net.alasc.ptrcoll

import scala.{specialized => sp}

trait KeysRemovable[@sp(Int, Long) A] extends Findable[A] { lhs =>
  /** Removes any value associated with key, and returns whether
    * an operation was performed.
    */
  def remove(key: A): Boolean = {
    val ptr = findPointerAt(key)
    val isValid = PtrTC.hasAt(ptr)
    if (isValid)
      removeAt(ptr.asInstanceOf[ValidPtr])
    isValid
  }

  /** Removes the pointed element. */
  def removeAt(ptr: ValidPtr): Unit

  /** Removes the pointed element, and returns the pointer
    * to the next element.
    */
  def removeAndAdvance(ptr: ValidPtr): Ptr = {
    val nextPtr = PtrTC.next(ptr)
    removeAt(ptr)
    nextPtr
  }


  /** Removes item from collection. Calls `remove`. */
  def -=(item: A): lhs.type = { remove(item); lhs }

  def --=(pt: PointableAt[A]): lhs.type = {
    import syntax.all._
    import pt.{PtrTC => ptPtrTC}
    var ptr = pt.pointer
    while (ptr.hasAt) {
      lhs.remove(ptr.at)
      ptr = ptr.next
    }
    lhs
  }
}
