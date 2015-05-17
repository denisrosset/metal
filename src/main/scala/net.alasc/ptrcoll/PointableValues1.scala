package net.alasc.ptrcoll

import scala.{specialized => sp}

trait PointableValues1[@sp(Int, Long) V1] extends Pointable {
  /** Returns the value of the object pointed by the pointer. */
  def ptrVal1(ptr: ValidPtr): V1
}
