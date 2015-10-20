package metal

import scala.math.max
import scala.reflect.{ClassTag, classTag}
import spire.algebra.Order

import syntax._

trait BitSet[K] extends SortedSet[K] {

  def words: Array[Long]

  def copy: BitSet[K]

}

final class BitSetImpl(var words: Array[Long], var wordSize: Int) extends BitSet[Int] {

  def orderK = spire.std.int.IntAlgebra

  def ctK = classTag[Int]

  def copy: BitSet[Int] = new BitSetImpl(words.clone, wordSize)

  def ptr: Ptr[Tag] = {
    var w = 0
    while(w < wordSize && words(w) == 0L) {
      w += 1
    }
    if (w == wordSize) return Ptr.Null[Tag]
    val index = w * 8 + java.lang.Long.numberOfTrailingZeros(words(w))
    Ptr[Tag](index)
  }

  def ptrFind[@specialized L](keyL: L): Ptr[Tag] = {
    val key = keyL.asInstanceOf[Int]
    val w = key >>> 3
    val bit = key & 0x7
    val contained = w < wordSize && (words(w) & (1 << bit)) != 0
    if (contained) Ptr[Tag](key) else Ptr.Null[Tag]
  }

  def ptrNext(ptr: VPtr[Tag]): Ptr[Tag] = {
    var w = ptr.v.toInt >>> 3
    var bit = (ptr.v & 0x7).toInt
    val nextBit = Util.nextBitAfter(words(w), bit)
    if (nextBit >= 0) return VPtr[Tag](ptr.v - bit + nextBit)
    w += 1
    if (w == wordSize) return Ptr.Null[Tag]
    while(w < wordSize && words(w) == 0L) {
      w += 1
    }
    if (w == wordSize) return Ptr.Null[Tag]
    val index = w * 8 + java.lang.Long.numberOfTrailingZeros(words(w))
    Ptr[Tag](index)
  }

  def ptrKey[@specialized L](ptr: VPtr[Tag]): L = ptr.v.toInt.asInstanceOf[L]

  def ptrAddKey[@specialized L](keyL: L): VPtr[Tag] = {
    val key = keyL.asInstanceOf[Int]
    val w = key >>> 3
    val bit = key & 0x7
    if (w >= words.length) {
      val newWords = new Array[Long](Util.nextPowerOfTwo(w + 1))
      java.lang.System.arraycopy(words, 0, newWords, 0, wordSize)
      words = newWords
    }
    words(w) |= (1 << bit)
    wordSize = max(wordSize, w + 1)
    VPtr[Tag](key)
  }

  def ptrRemove(ptr: VPtr[Tag]): Unit = {
    val w = ptr.v.toInt >>> 3
    val bit = ptr.v & 0x7
    if (w >= wordSize) return
    val masked = words(w) & (1 << bit)
    words(w) -= masked
  }

  def ptrRemoveAndAdvance(ptr: VPtr[Tag]): Ptr[Tag] = {
    val nextPtr = ptrNext(ptr)
    ptrRemove(ptr)
    nextPtr
  }

  def size: Int = {
    var count = 0
    var w = 0
    while(w < wordSize) {
      count += java.lang.Long.bitCount(words(w))
      w += 1
    }
    count
  }

  def isEmpty: Boolean = {
    var w = 0
    while (w < wordSize) {
      if (words(w) != 0L) return false
      w += 1
    }
    true
  }

  def nonEmpty: Boolean = !isEmpty

}

object BitSet extends SetFactory[Int, Dummy] {

  @inline final def startSize = 2

  def empty[K:ClassTag:Dummy:LBEv]: BitSet[K] = new BitSetImpl(
    words = new Array[Long](startSize),
    wordSize = 0).asInstanceOf[BitSet[K]]

  def apply[K:ClassTag:Dummy:LBEv](items: K*): BitSet[K] = {
    val s = empty[Int]
    items.foreach { a => s += a.asInstanceOf[Int] }
    s.asInstanceOf[BitSet[K]]
  }

  private[metal] def ofAllocatedWordSize[K:ClassTag:Dummy:LBEv](nWords: Int): BitSet[K] = new BitSetImpl(
    words = new Array[Long](nWords),
    wordSize = 0).asInstanceOf[BitSet[K]]

  def ofSize[K:ClassTag:Dummy:LBEv](n: Int): BitSet[K] =
    ofAllocatedWordSize(scala.math.max(startSize, n / 8))

}
