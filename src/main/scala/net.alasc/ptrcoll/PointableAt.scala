package net.alasc.ptrcoll

import scala.annotation.tailrec
import scala.{specialized => sp}

trait PointableAt[@sp(Int) A] extends Pointable with WithPointerAt[A]

trait PointableAtImpl[@specialized(Int) A] extends PointableAt[A] with HasPtrAt[A, RawPtr] { self =>
  def at(ptr: Long): A
  implicit def PtrTC: HasPtrAt[A, Ptr] = self.asInstanceOf[HasPtrAt[A, Ptr]]
}
