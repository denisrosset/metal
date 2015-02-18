package net.alasc.ptrcoll

/** Iterable-like trait for fast unboxed iteration over collections. */
trait Pointable { self =>
  /** Tag of this instance, used to select the correct typeclass for
    * the pointer syntax. */
  trait Tag
  /** Tagged pointer type for this collection instance. */
  type Ptr = TaggedPtr[Tag]
  def nullPtr: Ptr
  final def Ptr(r: RawPtr): Ptr = r.asInstanceOf[Ptr]
  /** Returns a pointer for this collection instance. */
  def pointer: Ptr
  /** Pointer typeclass. */
  implicit def PtrTC: HasPtr[Ptr]
}

trait PointableAt[@specialized(Int) A] extends Pointable {
  implicit def PtrTC: HasPtrAt[A, Ptr]
}

trait PointableImpl extends Pointable with HasPtr[RawPtr] { self =>
  def next(ptr: RawPtr): RawPtr
  def hasAt(ptr: RawPtr): Boolean
  /** Pointable is its own typeclass (magic!). */
  def PtrTC: HasPtr[Ptr] = self.asInstanceOf[HasPtr[Ptr]]
}

trait PointableAtImpl[@specialized(Int) A] extends PointableAt[A] with HasPtrAt[A, RawPtr] { self =>
  def at(ptr: Long): A
  implicit def PtrTC: HasPtrAt[A, Ptr] = self.asInstanceOf[HasPtrAt[A, Ptr]]
}
