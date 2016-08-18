package metal
package generic

import scala.reflect.ClassTag

import spire.algebra.Order
import spire.math.min
import spire.syntax.cfor._

abstract class BitSet extends SortedSet[Int] { lhs =>

  import generic.BitSet.{WordLength, LogWL}

  type Immutable = metal.immutable.BitSet
  type Mutable = metal.mutable.BitSet
  type Scala = scala.collection.immutable.BitSet

  def order: Order[Int] = spire.std.int.IntAlgebra

  def nWords: Int
  def word(i: Int): Long = words(i)

  protected def words: Array[Long]

  def ctK = ClassTag.Int
  def K = MetalTag.Int

  def mutableCopy = new mutable.ResizableBitSet(words.clone, nWords)

  override def priorityEquals = true

  override def ptrHash(ptr: VPtr[this.type]): Int = ptr.raw.toInt

  override def ptrToString(ptr: VPtr[this.type]): String = ptr.raw.toInt.toString

  override def ptrEquals(thisPtr: VPtr[this.type], that: generic.Set[Int]): Boolean =
    that.ptrFind[Int](thisPtr.raw.toInt).nonNull

  def keyArray(ptr: VPtr[this.type]) = sys.error("Cannot call keyArray on BitSet")

  def keyIndex(ptr: VPtr[this.type]) = sys.error("Cannot call keyArray on BitSet")

  def longSize: Long = {
    var count = 0L
    var w = 0
    while(w < nWords) {
      count += java.lang.Long.bitCount(words(w))
      w += 1
    }
    count
  }

  def isEmpty: Boolean = {
    var w = 0
    while (w < nWords) {
      if (words(w) != 0L) return false
      w += 1
    }
    true
  }

  def nonEmpty: Boolean = !isEmpty

  def ptr: Ptr[this.type] = {
    var w = 0
    while(w < nWords && words(w) == 0L) {
      w += 1
    }
    if (w == nWords) return Ptr.Null(this)
    val index = w * WordLength + java.lang.Long.numberOfTrailingZeros(words(w))
    Ptr(this, index)
  }

  def ptrFind[@specialized L](keyL: L): Ptr[this.type] = {
    val key = keyL.asInstanceOf[Int]
    val w = key >>> LogWL
    val contained = (w < nWords) && ((words(w) & (1L << key)) != 0)
    if (contained) Ptr(this, key) else Ptr.Null(this)
  }

  def findOrNextAfter[@specialized L](keyL: L): Ptr[this.type] = {
    val key = keyL.asInstanceOf[Int]
    var w = key >>> LogWL
    if (w >= nWords)
      return Ptr.Null(this)
    var word = words(w) & ((-1L) << key)
    while (true) {
      if (word != 0)
        return Ptr(this, (w * WordLength) + java.lang.Long.numberOfTrailingZeros(word))
      w += 1
      if (w == nWords)
        return Ptr.Null(this)
      word = words(w)
    }
    Ptr.Null(this)
  }

  def findOrNextBefore[@specialized L](keyL: L): Ptr[this.type] = {
    val key = keyL.asInstanceOf[Int]
    var w = key >>> LogWL
    var word = 0L
    if (w >= nWords) {
      w = nWords - 1
      word = words(w)
    } else
      word = words(w) & ((-1L) >>> key)
    while (true) {
      if (word != 0)
        return Ptr(this, (w * WordLength) + 63 - java.lang.Long.numberOfLeadingZeros(word))
      w -= 1
      if (w == -1)
        return Ptr.Null(this)
      word = words(w)
    }
    Ptr.Null(this)
  }

  def ptrNext(ptr: VPtr[this.type]): Ptr[this.type] = {
    val from = ptr.raw.toInt + 1
    var w = from >>> LogWL
    if (w >= nWords)
      return Ptr.Null(this)
    var word = words(w) & ((-1L) << from)
    while (true) {
      if (word != 0)
        return Ptr(this, (w * WordLength) + java.lang.Long.numberOfTrailingZeros(word))
      w += 1
      if (w == nWords)
        return Ptr.Null(this)
      word = words(w)
    }
    return Ptr.Null(this) // unreachable
  }

  def ptrKey[@specialized L](ptr: VPtr[this.type]): L = ptr.raw.toInt.asInstanceOf[L]

  def ptrElement1[@specialized E1](ptr: VPtr[this.type]): E1 = ptr.raw.toInt.asInstanceOf[E1]

  def isDisjoint(rhs: generic.BitSet): Boolean = {
    cforRange(0 until min(lhs.nWords, rhs.nWords)) { w =>
      if ((lhs.words(w) & rhs.words(w)) != 0L) return false
    }
    true
  }

}

object BitSet {

  val LogWL = 6
  val WordLength = 64

  import java.lang.Long.numberOfTrailingZeros

  /** Returns the number of words needed to store elements in 0 ... n-1. */
  def nWordsForSize(n: Int) =
    if (n == 0) 0 else ((n - 1) / WordLength) + 1


  /** Returns the minimal element of the bitset (lhs diff rhs) if non-empty, or -1. */
  def minOfDifference(lhs: generic.BitSet, rhs: generic.BitSet): Int = {
    var w = 0
    while (w < min(lhs.nWords, rhs.nWords)) {
      val word = lhs.words(w) & ~rhs.words(w)
      if (word != 0L)
        return w * WordLength + numberOfTrailingZeros(word)
      w += 1
    }
    while (w < lhs.nWords) {
      val word = lhs.words(w)
      if (word != 0L)
        return w * WordLength + numberOfTrailingZeros(word)
    }
    -1
  }

  /** Returns the minimal element of the bitset (lhs diff rhs) strictly greater than i or -1. */
  def nextOfDifference(i: Int, lhs: generic.BitSet, rhs: generic.BitSet): Int = {
    val from = i + 1
    var w = from / 64
    if (w >= lhs.nWords)
      return -1
    var word = (lhs.words(w) & ~rhs.words(w)) & ((-1L) << from)
    while (true) {
      if (word != 0)
        return w * WordLength + numberOfTrailingZeros(word)
      w += 1
      if (w == lhs.nWords) {
        while (w < lhs.nWords) {
          word = lhs.words(w)
          if (word != 0)
            return w * WordLength + numberOfTrailingZeros(word)
        }
        return -1
      }
      word = lhs.words(w) & ~rhs.words(w)
    }
    -1
  }

  /** Returns the minimal element of the bitset (lhs intersect rhs) if non-empty, or -1. */
  def minOfIntersection(lhs: generic.BitSet, rhs: generic.BitSet): Int = {
    var w = 0
    val nWords = min(lhs.nWords, rhs.nWords)
    while (w < nWords && (lhs.words(w) & rhs.words(w)) == 0L) {
      w += 1
    }
    if (w == nWords) return -1
    w * 64 + numberOfTrailingZeros((lhs.words(w) & rhs.words(w)))
  }

  /** Returns the minimal element of the bitset (lhs intersect rhs) strictly greater than i or -1. */
  def nextOfIntersection(i: Int, lhs: generic.BitSet, rhs: generic.BitSet): Int = {
    val from = i + 1
    var w = from / 64
    val nWords = min(lhs.nWords, rhs.nWords)
    if (w >= nWords)
      return -1
    var word = (lhs.words(w) & rhs.words(w)) & ((-1L) << from)
    while (true) {
      if (word != 0)
        return (w * 64) + numberOfTrailingZeros(word)
      w += 1
      if (w == nWords) return -1
      word = lhs.words(w) & rhs.words(w)
    }
    -1
  }

}

abstract class BitSetBuilder[B <: generic.BitSet] extends SetBuilder[Int, B] {

  import BitSet.nWordsForSize

  /** Builds a bitset from the given word array; only words 0 ... nWords-1 are nonzero. */
  def fromBitmaskNoCopy(words: Array[Long], nWords: Int): B

  /** Builds a bitset containing the indices 0 ... n-1. */
  def zeroUntil(n: Int): B = {
    val words = new Array[Long](nWordsForSize(n))
    cforRange(0 until (n / 64)) { w => words(w) = -1L }
    if (n % 64 != 0) {
      words(n / 64) = (1L << (n % 64)) - 1
    }
    fromBitmaskNoCopy(words, words.length)
  }

}
