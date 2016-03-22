package metal

package object util {

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
