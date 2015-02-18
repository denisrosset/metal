package net.alasc.ptrcoll
package sets

import scala.math.max
import scala.reflect.{ClassTag, classTag}

trait BitSSet extends SortedSSet[Int] {
  def ct = classTag[Int]
  def order = spire.std.int.IntAlgebra
  def words: Array[Long]
}

trait BitSSetImpl extends BitSSet with PointableAtImpl[Int] { self =>
  var words: Array[Long]
  var wordSize: Int

  def pointer: Ptr = {
    var w = 0
    while(words(w) == 0L) {
      if (w == wordSize) return Ptr(-1)
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

  def hasAt(ptr: RawPtr): Boolean = ptr != -1
  def next(ptr: RawPtr): RawPtr = {
    var w = ptr.toInt >>> 3
    var bit = (ptr & 0x7).toInt
    val nextBit = Util.nextBitAfter(words(w), bit)
    if (nextBit >= 0) return (ptr - bit + nextBit)
    w += 1
    if (w == wordSize) return Ptr(-1)
    while(words(w) == 0L) {
      if (w == wordSize) return Ptr(-1)
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
      val newWords = new Array[Long](Util.nextPowerOfTwo(w))
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

object BitSSet {
  @inline final def startSize = 2
  def empty: BitSSet = new BitSSetImpl {
    var words = new Array[Long](startSize)
    var wordSize = 0
  }
  def apply(items: Int*): BitSSet = {
    val s = empty
    items.foreach { a => s += a }
    s
  }
}
