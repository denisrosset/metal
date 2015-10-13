package metal

import scala.{specialized => sp}
import scala.annotation.tailrec

trait Removable[K] extends Keys[K] { lhs =>

  /** Removes the pointed element. */
  def ptrRemove(ptr: VPtr[Tag]): Unit

  /** Removes the pointed element, and returns the pointer
    * to the next element, or null if at the end.
    */
  def ptrRemoveAndAdvance(ptr: VPtr[Tag]): Ptr[Tag]

}
