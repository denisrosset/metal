package net.alasc.ptrcoll

import scala.annotation.tailrec
import scala.{specialized => sp}

/** Iterable-like trait for fast unboxed iteration over collections. */
trait Pointable[@sp(Int, Long) K] extends WithPointer[K] {
  /** Returns a pointer for this collection instance. */
  def pointer: Ptr
  /** Returns true if the collection is empty, false otherwise. */
  def isEmpty: Boolean
  /** Returns true if the collection is non-empty, false otherwise. */
  def nonEmpty: Boolean
  /**
    * Return the size of this collection as an Int.
    * 
    * Since most collections use arrays, their size is limited to what a 32-bit
    * signed integer can represent.
    */
  def size: Int
}
