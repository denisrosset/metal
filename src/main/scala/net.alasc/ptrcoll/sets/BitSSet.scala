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

class BitSSetImpl(var words: Array[Long], var wordSize: Int) extends BitSSet[Int] with Keyed[Int] with HasPtrAt[Int, RawPtr] { self =>
  def orderK = spire.std.int.IntAlgebra
  def ctK = classTag[Int]

  def copy: BitSSet[Int] = new BitSSetImpl(words.clone, wordSize)

  @inline final def nullPtr: Ptr = Ptr(-1)
  def pointer: Ptr = {
    var w = 0
    while(w < wordSize && words(w) == 0L) {
      w += 1
    }
    if (w == wordSize) return nullPtr
    val index = w * 8 + java.lang.Long.numberOfTrailingZeros(words(w))
    Ptr(index)
  }
  @inline final def contains(item: Int): Boolean = {
    val w = item >>> 3
    val bit = item & 0x7
    w < wordSize && (words(w) & (1 << bit)) != 0
  }
  final def findPointerAt(item: Int): Ptr =
    if (contains(item)) Ptr(item) else nullPtr
  final def hasAt(ptr: RawPtr): Boolean = ptr >= 0L
  final def nextPtr(ptr: RawPtr): RawPtr = {
    var w = ptr.toInt >>> 3
    var bit = (ptr & 0x7).toInt
    val nextBit = Util.nextBitAfter(words(w), bit)
    if (nextBit >= 0) return (ptr - bit + nextBit)
    w += 1
    if (w == wordSize) return nullPtr
    while(w < wordSize && words(w) == 0L) {
      w += 1
    }
    if (w == wordSize) return nullPtr
    val index = w * 8 + java.lang.Long.numberOfTrailingZeros(words(w))
    Ptr(index)
  }
  final def at(ptr: RawPtr) = ptr.toInt
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
  final def removeAndAdvance(ptr: ValidPtr): Ptr = {
    val nextPtr = PtrTC.nextPtr(ptr)
    removeAt(ptr)
    nextPtr
  }
  final def removeAt(ptr: ValidPtr): Unit = remove(ptr.toInt)
  final def isEmpty: Boolean = {
    var w = 0
    while (w < wordSize) {
      if (words(w) != 0L) return false
      w += 1
    }
    true
  }
  final def nonEmpty: Boolean = !isEmpty
  final def size: Int = {
    var count = 0
    var w = 0
    while(w < wordSize) {
      count += java.lang.Long.bitCount(words(w))
      w += 1
    }
    count
  }
  @inline final def Ptr(rawPtr: RawPtr) = rawPtr.asInstanceOf[Ptr]
  @inline final implicit def PtrTC: HasPtrAt[Int, Ptr] = self.asInstanceOf[HasPtrAt[Int, Ptr]]
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
