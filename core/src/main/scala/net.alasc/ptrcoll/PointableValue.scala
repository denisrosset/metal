package net.alasc.ptrcoll

import scala.{specialized => sp}

trait PointableValue[@sp(Int, Long) V] extends Pointable {
  /** Returns the value of the object pointed by the pointer. */
  def ptrValue(ptr: VPtr[Tag]): V
}
