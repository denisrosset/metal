package net.alasc.ptrcoll
package sets

import scala.{specialized => sp}
import scala.math.max
import scala.reflect.{ClassTag, classTag}
import spire.algebra.Order

trait BitSSet[@sp(Int) A] extends SortedSSet[A] {
  def order: Order[A]
  def words: Array[Long]
}

trait BitSSetImpl extends BitSSet[Int] with PointableAtImpl[Int] { self =>
  def order = spire.std.int.IntAlgebra
  def ct = classTag[Int]
  var words: Array[Long]
  var wordSize: Int
  @inline final def nullPtr: Ptr = Ptr(-1)
  def pointer: Ptr = {
    var w = 0
    while(words(w) == 0L) {
      if (w == wordSize) return nullPtr
      w += 1
    }
    val index = w * 8 + java.lang.Long.numberOfTrailingZeros(words(w))
    Ptr(index)
  }
  def apply(item: Int): Boolean = {
    val w = item >>> 3
    val bit = item & 0x7
    w < wordSize && (words(w) & (1 << bit)) != 0
  }
  def findPointerAt(item: Int): Ptr =
    if (apply(item)) Ptr(item) else nullPtr
  def hasAt(ptr: RawPtr): Boolean = ptr >= 0L
  def next(ptr: RawPtr): RawPtr = {
    var w = ptr.toInt >>> 3
    var bit = (ptr & 0x7).toInt
    val nextBit = Util.nextBitAfter(words(w), bit)
    if (nextBit >= 0) return (ptr - bit + nextBit)
    w += 1
    if (w == wordSize) return nullPtr
    while(words(w) == 0L) {
      if (w == wordSize) return nullPtr
      w += 1
    }
    val index = w * 8 + java.lang.Long.numberOfTrailingZeros(words(w))
    Ptr(index)
  }
  def at(ptr: RawPtr) = ptr.toInt
  def add(item: Int): Boolean = {
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
  def remove(item: Int): Boolean = {
    val w = item.toInt >>> 3
    val bit = item & 0x7
    if (w >= wordSize) return false
    val masked = words(w) & (1 << bit)
    words(w) -= masked
    masked != 0
  }
  def removeAt(ptr: Ptr): Ptr = {
    val nextPtr = PtrTC.next(ptr)
    if (hasAt(ptr)) remove(at(ptr))
    nextPtr
  }
  def size: Int = {
    var count = 0
    var w = 0
    while (w < wordSize) {
      count += java.lang.Long.bitCount(words(w))
      w += 1
    }
    count
  }
}

object BitSSet extends SSetFactory[Int, Dummy] {
  @inline final def startSize = 2
  def empty[@sp(Int) A: ClassTag: Dummy: LBEv]: BitSSet[A] = (new BitSSetImpl {
    var words = new Array[Long](startSize)
    var wordSize = 0
  }).asInstanceOf[BitSSet[A]]
  def apply[@sp(Int) A: ClassTag: Dummy: LBEv](items: A*): BitSSet[A] = ({
    val s = empty[Int]
    items.foreach { a => s += a.asInstanceOf[Int] }
    s
  }).asInstanceOf[BitSSet[A]]
  private[ptrcoll] def ofAllocatedWordSize[@sp(Int) A: ClassTag: Dummy: LBEv](nWords: Int)(implicit ev: A <:< Int): BitSSet[A] = (new BitSSetImpl {
    var words = new Array[Long](nWords)
    var wordSize = 0
  }).asInstanceOf[BitSSet[A]]
  def ofSize[@sp(Int) A: ClassTag: Dummy: LBEv](n: Int): BitSSet[A] =
    ofAllocatedWordSize(scala.math.max(startSize, n / 8))
}
