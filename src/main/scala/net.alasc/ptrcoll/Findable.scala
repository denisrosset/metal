package net.alasc.ptrcoll

import scala.{specialized => sp}

trait Findable[@sp(Int, Long) A] extends PointableAt[A] {
  /** Returns a pointer to the given item, if it exists. */
  def findPointerAt(item: A): Ptr
}
