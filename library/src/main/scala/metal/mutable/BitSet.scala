package metal
package mutable

import scala.annotation.tailrec

import spire.syntax.cfor._
import spire.math.min

abstract class BitSet extends generic.BitSet with mutable.SortedSet[Int] {

  def words: Array[Long]

  def reset(): Unit = {
    cforRange(0 until nWords) { w =>
      words(w) = 0L
    }
  }

  def toImmutable: Immutable = new immutable.BitSet(words.clone) // TODO: trim array

  def toScala = toImmutable.toScala

  def ptrRemoveAndAdvance(ptr: VPtr[this.type]): Ptr[this.type] = {
    val nextPtr = ptrNext(ptr)
    ptrRemove(ptr)
    nextPtr
  }

  def &=(other: generic.Set[Int]): this.type = other match {
    case rhs: generic.BitSet =>
      cforRange(0 until min(nWords, rhs.nWords)) { w =>
        words(w) &= rhs.word(w)
      }
      if (rhs.nWords < nWords)
        java.util.Arrays.fill(words, rhs.nWords, nWords, 0L) // zero in remaining words
      this
    case _ =>
      @tailrec def rec(ptr: Ptr[this.type]): Unit = ptr match {
        case IsVPtr(vp) =>
          val i = ptrKey[Int](vp)
          if (other.ptrFind[Int](i).nonNull)
            rec(ptrNext(vp))
          else
            rec(ptrRemoveAndAdvance(vp))
        case _ =>
      }
      rec(this.ptr)
      this
  }

  def &~=(other: generic.Set[Int]): this.type = other match {
    case rhs: generic.BitSet =>
      cforRange(0 until min(nWords, rhs.nWords)) { w =>
        words(w) &= ~rhs.word(w)
      }
      this
    case _ =>
      @tailrec def rec(ptr: Ptr[this.type]): Unit = ptr match {
        case IsVPtr(vp) =>
          val i = ptrKey[Int](vp)
          if (other.ptrFind[Int](i).nonNull)
            rec(ptrRemoveAndAdvance(vp))
          else
            rec(ptrNext(vp))
        case _ =>
      }
      rec(this.ptr)
      this
  }

  def |=(other: generic.Set[Int]): this.type = {
    @tailrec def rec(ptr: Ptr[other.type]): Unit = ptr match {
      case IsVPtr(vp) =>
        val i = other.ptrKey[Int](vp)
        ptrAddKey[Int](i)
        rec(other.ptrNext(vp))
      case _ =>
    }
    rec(other.ptr)
    this
  }

}

/** Bitset represented by an array of longs, each `Long` containing the information
  * about the membership of 64 integers. The elements are all non-negative.
  * 
  * The part of words(i >= nWords) must always be 0L.
  */
final class ResizableBitSet(var words: Array[Long], var nWords: Int) extends mutable.BitSet {

  import generic.BitSet.LogWL

  def clear(): Unit = {
    words = new Array[Long](mutable.BitSet.startSize)
    nWords = 0
  }

  def result(): Immutable = {
    val res = new immutable.BitSet(words)
    words = new Array[Long](0)
    nWords = 0
    res
  }

  def resizeTo(maxWords: Int): Unit = {
    val newWords = new Array[Long](maxWords)
    java.lang.System.arraycopy(words, 0, newWords, 0, nWords)
    words = newWords
  }

  def ptrAddKey[@specialized L](keyL: L): VPtr[this.type] = {
    val key = keyL.asInstanceOf[Int]
    val w = key >>> LogWL
    if (w >= words.length)
      resizeTo(util.nextPowerOfTwo(w + 1))
    words(w) |= (1L << key)
    nWords = scala.math.max(nWords, w + 1)
    VPtr(this, key)
  }

  def ptrRemove(ptr: VPtr[this.type]): Unit = {
    val i = ptr.raw.toInt
    val w = i >>> LogWL
    if (w >= nWords) return
    words(w) &= ~(1L << i)
  }

  override def |=(other: generic.Set[Int]): this.type = other match {
    case rhs: generic.BitSet =>
      if (rhs.nWords > nWords)
        resizeTo(rhs.nWords)
      cforRange(0 until rhs.nWords) { w =>
        words(w) |= rhs.word(w)
      }
      this
    case _ => super.|=(other)
  }

}

/** Bitset represented by an array of longs, each `Long` containing the information
  * about the membership of 64 integers. The elements are all non-negative.
  * 
  * The size of this BitSet is set at creation time and is never modified.
  */
final class FixedBitSet(var words: Array[Long]) extends mutable.BitSet {

  import generic.BitSet.LogWL

  def nWords = words.length

  /** Equivalent to [[reset]], because deallocating does not make sense. */
  def clear(): Unit = reset()

  def result(): Immutable = {
    val res = new immutable.BitSet(words)
    words = new Array[Long](words.length)
    res
  }

  def ptrAddKey[@specialized L](keyL: L): VPtr[this.type] = {
    val key = keyL.asInstanceOf[Int]
    val w = key >>> LogWL
    words(w) |= (1L << key)
    VPtr(this, key)
  }

  def ptrRemove(ptr: VPtr[this.type]): Unit = {
    val i = ptr.raw.toInt
    val w = i >>> LogWL
    words(w) &= ~(1L << i)
  }

  override def |=(other: generic.Set[Int]): this.type = other match {
    case rhs: generic.BitSet =>
      cforRange(rhs.nWords - 1 to 0 by -1) { w =>
        val word = rhs.word(w)
        if (word != 0L)
          words(w) |= word
      }
      this
    case _ => super.|=(other)
  }


}

object BitSet extends mutable.SetBuilder[Int, mutable.BitSet] {

  import generic.BitSet.WordLength

  @inline final def startSize = 2

  /** Returns the number of words needed to store elements in 0 ... n-1. */
  def nWordsForSize(n: Int) =
    if (n == 0) 0 else ((n - 1) / WordLength) + 1

  def ofAllocatedWordSize(nWords: Int): mutable.BitSet =
    new mutable.ResizableBitSet(new Array[Long](nWords), 0)

  /** Returns a bitset with sufficient space to store elements in 0 ... n-1 without further allocations. */
  def reservedSize(n: Long): mutable.BitSet = {
    require(n.isValidInt)
    ofAllocatedWordSize(spire.math.max(startSize, nWordsForSize(n.toInt)))
  }

  /** Returns a fixed size bitset able to store elements in 0 ... n-1. */
  def fixedSize(n: Long): mutable.BitSet = {
    require(n.isValidInt)
    new mutable.FixedBitSet(new Array[Long](nWordsForSize(n.toInt)))
  }

  override def fromIterable(items: Iterable[Int]): mutable.BitSet = items match {
    case bs1: scala.collection.immutable.BitSet.BitSet1 => new mutable.ResizableBitSet(Array(bs1.elems), 1)
    case bsn: scala.collection.immutable.BitSet.BitSetN => new mutable.ResizableBitSet(bsn.elems, bsn.elems.length)
    case bs: scala.collection.BitSet =>
      val bm = bs.toBitMask
      new mutable.ResizableBitSet(bm, bm.length)
    case _ => super.fromIterable(items)
  }

}
