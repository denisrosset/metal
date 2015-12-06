package metal

import scala.{specialized => sp}
import scala.annotation.tailrec

trait Removable extends Nextable { self =>

  type Cap <: Removable

  /** Removes the pointed element. */
  def ptrRemove(ptr: MyVPtr): Unit

  /** Removes the pointed element, and returns the pointer
    * to the next element, or null if at the end.
    */
  def ptrRemoveAndAdvance(ptr: MyVPtr): MyPtr

  def clear(): Unit

}
