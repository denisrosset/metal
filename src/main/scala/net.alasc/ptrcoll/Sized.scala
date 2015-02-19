package net.alasc.ptrcoll

import scala.{specialized => sp}

trait Sized {
  /**
    * Return the size of this collection as an Int.
    * 
    * Since most collections use arrays, their size is limited to what a 32-bit
    * signed integer can represent.
    */
  def size: Int

  /**
    * Return true if the collection is empty, false otherwise.
    */
  def isEmpty: Boolean = size == 0

  /**
    * Return true if the collection is non-empty, false otherwise.
    * 
    * This is an O(1) operation.
    */
  def nonEmpty: Boolean = size > 0
}

trait Findable[@sp(Int) A] extends PointableAt[A] {
  /** Returns a pointer to the given item, if it exists. */
  def findPointerAt(item: A): Ptr
}
