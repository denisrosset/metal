package net.alasc.ptrcoll

import scala.{specialized => sp}

trait PointableValue2[@sp(Int, Long) V2] extends Pointable {
  /** Returns the value of the object pointed by the pointer. */
  def ptrValue2(ptr: VPtr[Tag]): V2
}
