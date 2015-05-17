package net.alasc.ptrcoll

import scala.{specialized => sp}

trait Pointable {
  /** Tag of this instance, used to select the correct typeclass for
    * the pointer syntax. */
  trait Tag
  /** Tagged pointer type for this collection instance. */
  type Ptr = RawPtr[Tag, Validity]
  type ValidPtr = RawPtr[Tag, IsValid]
}
