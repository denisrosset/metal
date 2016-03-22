package metal
package mutable

import spire.math.max
import spire.syntax.cfor._
import generic.Methods

trait BitSet[K] extends generic.BitSet[K] with mutable.SortedSet[K]

final class BitSetImpl(var words: Array[Long], var nWords: Int) extends generic.BitSetImpl with mutable.BitSet[Int] {

  def clear(): Unit = {
    words = new Array[Long](metal.mutable.BitSet.startSize)
    nWords = 0
  }

  def reset(): Unit = {
    cforRange(0 until nWords) { w =>
      words(w) = 0L
    }
  }

  def result(): Immutable = {
    val res = new immutable.BitSetImpl(words, nWords)
    words = new Array[Long](0)
    nWords = 0
    res
  }

  def toImmutable: Immutable = new immutable.BitSetImpl(words.clone, nWords) // TODO: trim array

  def ptrAddKey[@specialized L](keyL: L): VPtr[this.type] = {
    val key = keyL.asInstanceOf[Int]
    val w = key >>> 3
    val bit = key & 0x7
    if (w >= words.length) {
      val newWords = new Array[Long](util.nextPowerOfTwo(w + 1))
      java.lang.System.arraycopy(words, 0, newWords, 0, nWords)
      words = newWords
    }
    words(w) |= (1 << bit)
    nWords = scala.math.max(nWords, w + 1)
    VPtr(this, key)
  }

  def ptrRemove(ptr: VPtr[this.type]): Unit = {
    val w = ptr.raw.toInt >>> 3
    val bit = ptr.raw & 0x7
    if (w >= nWords) return
    val masked = words(w) & (1 << bit)
    words(w) -= masked
  }

  def ptrRemoveAndAdvance(ptr: VPtr[this.type]): Ptr[this.type] = {
    val nextPtr = ptrNext(ptr)
    ptrRemove(ptr)
    nextPtr
  }

}

object BitSet extends mutable.SetFactory {

  type Extra[K] = K =:= Int

  type S[K] = mutable.BitSet[K]

  @inline final def startSize = 2

  def ofAllocatedWordSize[K:Methods:Extra](nWords: Int): S[K] =
    (new mutable.BitSetImpl(new Array[Long](nWords), 0)).asInstanceOf[S[K]]

  def reservedSize[K:Methods:Extra](n: Int): S[K] =
    ofAllocatedWordSize[K](max(startSize, (n + 7) / 8))

}
