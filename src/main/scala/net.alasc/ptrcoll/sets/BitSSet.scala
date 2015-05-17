package net.alasc.ptrcoll
package sets

import scala.{specialized => sp}
import scala.math.max
import scala.reflect.{ClassTag, classTag}
import spire.algebra.Order

trait BitSSet[@sp(Int) K] extends SortedSSet[K] {
  def words: Array[Long]
  def copy: BitSSet[K]
}

class BitSSetImpl(var words: Array[Long], var wordSize: Int) extends BitSSet[Int] {
  def orderK = spire.std.int.IntAlgebra
  def ctK = classTag[Int]

  def copy: BitSSet[Int] = new BitSSetImpl(words.clone, wordSize)

  def ptrStart: Ptr = {
    var w = 0
    while(w < wordSize && words(w) == 0L) {
      w += 1
    }
    if (w == wordSize) return NullPtr[Tag]
    val index = w * 8 + java.lang.Long.numberOfTrailingZeros(words(w))
    ValidPtr[Tag](index)
  }
  @inline final def contains(item: Int): Boolean = {
    val w = item >>> 3
    val bit = item & 0x7
    w < wordSize && (words(w) & (1 << bit)) != 0
  }
  final def ptrFind(item: Int): Ptr =
    if (contains(item)) ValidPtr[Tag](item) else NullPtr[Tag]
  final def ptrNext(ptr: ValidPtr): Ptr = {
    var w = ptr.v.toInt >>> 3
    var bit = (ptr.v & 0x7).toInt
    val nextBit = Util.nextBitAfter(words(w), bit)
    if (nextBit >= 0) return ValidPtr[Tag](ptr.v - bit + nextBit)
    w += 1
    if (w == wordSize) return NullPtr[Tag]
    while(w < wordSize && words(w) == 0L) {
      w += 1
    }
    if (w == wordSize) return NullPtr[Tag]
    val index = w * 8 + java.lang.Long.numberOfTrailingZeros(words(w))
    ValidPtr[Tag](index)
  }
  final def ptrKey(ptr: ValidPtr) = ptr.v.toInt
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
  final def ptrRemoveAndAdvance(ptr: ValidPtr): Ptr = {
    val nextPtr = ptrNext(ptr)
    ptrRemove(ptr)
    nextPtr
  }
  final def ptrRemove(ptr: ValidPtr): Unit = remove(ptr.v.toInt)
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

object BitSSet extends MutSSetFactory[Int, Dummy] {
  @inline final def startSize = 2
  def empty[@sp(Int) K: ClassTag: Dummy: LBEv]: BitSSet[K] = new BitSSetImpl(
    words = new Array[Long](startSize),
    wordSize = 0).asInstanceOf[BitSSet[K]]
  def apply[@sp(Int) K: ClassTag: Dummy: LBEv](items: K*): BitSSet[K] = {
    val s = empty[Int]
    items.foreach { a => s += a.asInstanceOf[Int] }
    s.asInstanceOf[BitSSet[K]]
  }
  private[ptrcoll] def ofAllocatedWordSize[@sp(Int) K: ClassTag: Dummy: LBEv](nWords: Int)(implicit ev: K <:< Int): BitSSet[K] = new BitSSetImpl(
    words = new Array[Long](nWords),
    wordSize = 0).asInstanceOf[BitSSet[K]]
  def ofSize[@sp(Int) K: ClassTag: Dummy: LBEv](n: Int): BitSSet[K] =
    ofAllocatedWordSize(scala.math.max(startSize, n / 8))
}
