package net.alasc.ptrcoll

import scala.{specialized => sp}

trait PointableKeys[@sp(Int, Long) K] extends Pointable {
  /** Returns the key pointed by `ptr`. */
  def ptrKey(ptr: ValidPtr): K
}
