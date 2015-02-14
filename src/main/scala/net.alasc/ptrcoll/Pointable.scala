package net.alasc.ptrcoll

/** Iterable-like trait for fast unboxed iteration over collections. */
trait Pointable[@specialized(Int, Long, Double) A] extends HasPtr[A, RawPtr] { self =>
  /** Tag of this instance, used to select the correct typeclass for
    * the pointer syntax. */
  trait Tag
  /** Tagged pointer type for this collection instance. */
  type Ptr = TaggedPtr[Tag]
  /** Returns a pointer for this collection instance. */
  def pointer: Ptr
  def next(ptr: RawPtr): RawPtr
  def hasAt(ptr: RawPtr): Boolean
  def at(ptr: RawPtr): A
  /** Pointable is its own typeclass (magic!). */
  implicit def HasPtr: HasPtr[A, Ptr] = self.asInstanceOf[HasPtr[A, Ptr]]
}
