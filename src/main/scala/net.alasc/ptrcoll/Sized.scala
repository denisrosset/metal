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
  def isEmpty: Boolean
  def nonEmpty: Boolean
}
