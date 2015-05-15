package net.alasc.ptrcoll

import scala.annotation.tailrec
import scala.{specialized => sp}
import scala.reflect.ClassTag

trait WithPointer[@sp(Int, Long) K] {
  implicit def ctK: ClassTag[K]
  /** Tag of this instance, used to select the correct typeclass for
    * the pointer syntax. */
  trait Tag
  /** Tagged pointer type for this collection instance. */
  type Ptr = TaggedPtr[Tag]
  type ValidPtr = ValidTaggedPtr[Tag]
  def nullPtr: Ptr
  def Ptr(r: RawPtr): Ptr
  /** Pointer typeclass. */
  implicit def PtrTC: HasPtrAt[K, Ptr]
}
