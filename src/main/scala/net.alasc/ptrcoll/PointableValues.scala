package net.alasc.ptrcoll

import scala.{specialized => sp}

trait PointableValues[@sp(Int, Long) V] extends Pointable {
  /** Returns the value of the object pointed by the pointer. */
  def ptrVal(ptr: ValidPtr): V
}
