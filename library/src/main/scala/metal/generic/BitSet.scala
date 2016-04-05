package metal
package generic

import scala.reflect.ClassTag

import spire.algebra.Order

abstract class BitSet extends SortedSet[Int] {

  import generic.BitSet.{WordLength, LogWL}

  type Immutable = metal.immutable.BitSet
  type Mutable = metal.mutable.BitSet

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

}

object BitSet {

  val LogWL = 6
  val WordLength = 64

}
