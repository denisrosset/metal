package metal
package generic

import spire.algebra.Order

abstract class BitSet[K] extends SortedSet[K] {

  implicit def ev: K =:= Int

  type Immutable = metal.immutable.BitSet[K]
  type Mutable = metal.mutable.BitSet[K]

  def nWords: Int
  def word(i: Int): Long

}

abstract class BitSetImpl extends BitSet[Int] {

  def words: Array[Long]

  def word(i: Int) = words(i)

  def ev = implicitly[=:=[Int, Int]]
  def order: Order[Int] = spire.std.int.IntAlgebra
  def K = Methods.Int

  def mutableCopy = new metal.mutable.BitSetImpl(words.clone, nWords)

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
    val index = w * 8 + java.lang.Long.numberOfTrailingZeros(words(w))
    Ptr(this, index)
  }

  def ptrFind[@specialized L](keyL: L): Ptr[this.type] = {
    val key = keyL.asInstanceOf[Int]
    val w = key >>> 3
    val bit = key & 0x7
    val contained = w < nWords && (words(w) & (1 << bit)) != 0
    if (contained) Ptr(this, key) else Ptr.Null(this)
  }

  def ptrNext(ptr: VPtr[this.type]): Ptr[this.type] = {
    var w = ptr.raw.toInt >>> 3
    var bit = (ptr.raw & 0x7).toInt
    val nextBit = util.nextBitAfter(words(w), bit)
    if (nextBit >= 0) return Ptr(this, ptr.raw - bit + nextBit)
    w += 1
    if (w == nWords) return Ptr.Null(this)
    while(w < nWords && words(w) == 0L) {
      w += 1
    }
    if (w == nWords) return Ptr.Null(this)
    val index = w * 8 + java.lang.Long.numberOfTrailingZeros(words(w))
    Ptr(this, index)
  }

  def ptrKey[@specialized L](ptr: VPtr[this.type]): L = ptr.raw.toInt.asInstanceOf[L]

  def ptrElement1[@specialized E1](ptr: VPtr[this.type]): E1 = ptr.raw.toInt.asInstanceOf[E1]

}
