package net.alasc.ptrcoll

import scala.annotation.tailrec
import scala.{specialized => sp}

trait WithPointer {
  /** Tag of this instance, used to select the correct typeclass for
    * the pointer syntax. */
  trait Tag
  /** Tagged pointer type for this collection instance. */
  type Ptr = TaggedPtr[Tag]
  type ValidPtr = ValidTaggedPtr[Tag]
  def nullPtr: Ptr
  final def Ptr(r: RawPtr): Ptr = r.asInstanceOf[Ptr]
  /** Pointer typeclass. */
  implicit def PtrTC: HasPtr[Ptr]
}

trait WithPointerAt[@sp(Int) A] extends WithPointer {
  implicit def PtrTC: HasPtrAt[A, Ptr]
}
