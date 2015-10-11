package net.alasc.ptrcoll

import scala.{specialized => sp}

trait PointableKey[@sp(Int, Long) K] extends Pointable {
  /** Returns the key pointed by `ptr`. */
  def ptrKey(ptr: VPtr[Tag]): K
}
