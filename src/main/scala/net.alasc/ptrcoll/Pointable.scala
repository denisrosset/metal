package net.alasc.ptrcoll

import scala.annotation.tailrec
import scala.{specialized => sp}

/** Iterable-like trait for fast unboxed iteration over collections. */
trait Pointable extends WithPointer {
  /** Returns a pointer for this collection instance. */
  def pointer: Ptr
  /** Returns true if the collection is empty, false otherwise. */
  def isEmpty: Boolean = !PtrTC.hasAt(pointer)
  /** Returns true if the collection is non-empty, false otherwise. */
  def nonEmpty: Boolean = PtrTC.hasAt(pointer)
}

trait PointableImpl extends Pointable with HasPtr[RawPtr] { self =>
  def next(ptr: RawPtr): RawPtr
  def hasAt(ptr: RawPtr): Boolean
  /** Pointable is its own typeclass (magic!). */
  def PtrTC: HasPtr[Ptr] = self.asInstanceOf[HasPtr[Ptr]]
}
