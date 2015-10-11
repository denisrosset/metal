package net.alasc.ptrcoll

import scala.{specialized => sp}

trait PointableValue1[@sp(Int, Long) V1] extends Pointable {
  /** Returns the value of the object pointed by the pointer. */
  def ptrValue1(ptr: VPtr[Tag]): V1
}
