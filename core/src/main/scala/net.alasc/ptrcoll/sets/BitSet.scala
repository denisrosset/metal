package net.alasc.ptrcoll
package sets

import scala.{specialized => sp}
import scala.math.max
import scala.reflect.{ClassTag, classTag}
import spire.algebra.Order

trait BitSet[@sp(Int) K] extends SortedSet[K] {

  def words: Array[Long]

  def copy: BitSet[K]

}

class BitSetImpl(var words: Array[Long], var wordSize: Int) extends BitSet[Int] {

  def orderK = spire.std.int.IntAlgebra

  def ctK = classTag[Int]

  def copy: BitSet[Int] = new BitSetImpl(words.clone, wordSize)

  def ptrStart: Ptr[Tag] = {
    var w = 0
    while(w < wordSize && words(w) == 0L) {
      w += 1
    }
    if (w == wordSize) return Ptr.Null[Tag]
    val index = w * 8 + java.lang.Long.numberOfTrailingZeros(words(w))
    VPtr[Tag](index)
  }

  @inline final def contains(item: Int): Boolean = {
    val w = item >>> 3
    val bit = item & 0x7
    w < wordSize && (words(w) & (1 << bit)) != 0
  }

  final def ptrFind(item: Int): Ptr[Tag] =
    if (contains(item)) VPtr[Tag](item) else Ptr.Null[Tag]

  final def ptrNext(ptr: VPtr[Tag]): Ptr[Tag] = {
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
    VPtr[Tag](index)
  }

  final def ptrKey(ptr: VPtr[Tag]) = ptr.v.toInt

  final def add(item: Int): Boolean = {
    val w = item.toInt >>> 3
    val bit = item & 0x7
    if (w >= words.length) {
      val newWords = new Array[Long](Util.nextPowerOfTwo(w + 1))
      java.lang.System.arraycopy(words, 0, newWords, 0, wordSize)
      words = newWords
    }
    val wasThere = (words(w) & (1 << bit)) != 0
    words(w) |= (1 << bit)
    wordSize = max(wordSize, w + 1)
    wasThere
  }

  final def remove(item: Int): Boolean = {
    val w = item.toInt >>> 3
    val bit = item & 0x7
    if (w >= wordSize) return false
    val masked = words(w) & (1 << bit)
    words(w) -= masked
    masked != 0
  }

  final def -=(item: Int): this.type = { remove(item); this }

  final def +=(item: Int): this.type = { add(item); this }

  final def ptrRemoveAndAdvance(ptr: VPtr[Tag]): Ptr[Tag] = {
    val nextPtr = ptrNext(ptr)
    ptrRemove(ptr)
    nextPtr
  }

  final def ptrRemove(ptr: VPtr[Tag]): Unit = remove(ptr.v.toInt)

  final def size: Int = {
    var count = 0
    var w = 0
    while(w < wordSize) {
      count += java.lang.Long.bitCount(words(w))
      w += 1
    }
    count
  }

  final def isEmpty: Boolean = {
    var w = 0
    while (w < wordSize) {
      if (words(w) != 0L) return false
      w += 1
    }
    true
  }

  final def nonEmpty: Boolean = !isEmpty

}

object BitSet extends MSetFactory[Int, Dummy] {

  @inline final def startSize = 2

  def empty[@sp(Int) K: ClassTag: Dummy: LBEv]: BitSet[K] = new BitSetImpl(
    words = new Array[Long](startSize),
    wordSize = 0).asInstanceOf[BitSet[K]]

  def apply[@sp(Int) K: ClassTag: Dummy: LBEv](items: K*): BitSet[K] = {
    val s = empty[Int]
    items.foreach { a => s += a.asInstanceOf[Int] }
    s.asInstanceOf[BitSet[K]]
  }

  private[ptrcoll] def ofAllocatedWordSize[@sp(Int) K: ClassTag: Dummy: LBEv](nWords: Int)(implicit ev: K <:< Int): BitSet[K] = new BitSetImpl(
    words = new Array[Long](nWords),
    wordSize = 0).asInstanceOf[BitSet[K]]

  def ofSize[@sp(Int) K: ClassTag: Dummy: LBEv](n: Int): BitSet[K] =
    ofAllocatedWordSize(scala.math.max(startSize, n / 8))

}
