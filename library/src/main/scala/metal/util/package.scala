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

}
