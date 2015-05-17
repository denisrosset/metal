package net.alasc.ptrcoll

import scala.{specialized => sp}

trait PointableValues2[@sp(Int, Long) V2] extends Pointable {
  /** Returns the value of the object pointed by the pointer. */
  def ptrVal2(ptr: ValidPtr): V2
}
