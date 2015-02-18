package net.alasc.ptrcoll

import scala.{specialized => sp}

case class PtrCollOverflowError(n: Int) extends Exception("size %s exceeds max" format n)
class KeyNotFoundException(k: String) extends Exception("key %s was not found" format k)

class Unit1[@sp A]

class Unit2[@sp A, @sp B]

object Util {
  /**
   * Given a number n, this method returns n if n is a power-of-two.
   * 
   * Otherwise, it returns the smallest power-of-two larger than n.
   */
  def nextPowerOfTwo(n: Int): Int = {
    val x = java.lang.Integer.highestOneBit(n)
    if (x == n) n else x * 2
  }

  /** Given a long word and a bit index, returns the next higher order
    * bit set, or returns -1.
    */
  def nextBitAfter(word: Long, currentBit: Int): Int = {
    val mask = (~0L) - ((1L << (currentBit + 1)) - 1L)
    val ind = java.lang.Long.numberOfTrailingZeros(word & mask)
    if (ind == 64) -1 else ind
  }
}
