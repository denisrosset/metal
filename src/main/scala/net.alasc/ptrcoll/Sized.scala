package net.alasc.ptrcoll

import scala.{specialized => sp}

trait Sized {
  /**
    * Return the size of this collection as an Int.
    * 
    * Since most collections use arrays, their size is limited to what a 32-bit
    * signed integer can represent.
    */
  def size: Int
}

trait Findable[@sp(Int) A] extends PointableAt[A] {
  /** Returns a pointer to the given item, if it exists. */
  def findPointerAt(item: A): Ptr
}
