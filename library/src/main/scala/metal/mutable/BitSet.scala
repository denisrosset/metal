package metal
package mutable

import spire.syntax.cfor._
import spire.math.min

abstract class BitSet extends generic.BitSet with mutable.SortedSet[Int] {

  import generic.BitSet.LogWL

  def words: Array[Long]

  def update(idx: Int, value: Boolean): Unit

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

  def &=(rhs: generic.BitSet): this.type = {
    cforRange(0 until min(nWords, rhs.nWords)) { w =>
      words(w) &= rhs.word(w)
    }
    if (rhs.nWords < nWords)
      java.util.Arrays.fill(words, rhs.nWords, nWords, 0L) // zero in remaining words
    this
  }

  def &~=(rhs: generic.BitSet): this.type = {
    cforRange(0 until min(nWords, rhs.nWords)) { w =>
      words(w) &= ~rhs.word(w)
    }
    this
  }

  def |=(other: generic.BitSet): this.type

}

object BitSet {

  @inline final def startSize = 2

}

abstract class BitSetBuilder[B <: mutable.BitSet] extends generic.BitSetBuilder[B] with mutable.SetBuilder[Int, B] {

  import generic.BitSet.nWordsForSize
  import mutable.BitSet.startSize

  def ofAllocatedWordSize(nWords: Int): B =
    fromBitmaskNoCopy(new Array[Long](nWords), 0)

  /** Returns a bitset with sufficient space to store elements in 0 ... n-1 without further allocations. */
  def reservedSize(n: Long): B = {
    require(n.isValidInt)
    ofAllocatedWordSize(spire.math.max(startSize, nWordsForSize(n.toInt)))
  }

  override def fromIterable(items: Iterable[Int]): B = items match {
    case bs1: scala.collection.immutable.BitSet.BitSet1 => fromBitmaskNoCopy(Array(bs1.elems), 1)
    case bsn: scala.collection.immutable.BitSet.BitSetN => fromBitmaskNoCopy(bsn.elems.clone, bsn.elems.length)
    case bs: scala.collection.BitSet =>
      val bm = bs.toBitMask
      fromBitmaskNoCopy(bm, bm.length)
    case _ =>
      val set = reservedSize(if (items.isEmpty) 0 else items.max + 1)
      items.foreach { k =>
        set.ptrAddKey(k)
      }
      set
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
    if (w >= nWords)
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

  override def |=(rhs: generic.BitSet): this.type = {
    if (rhs.nWords > nWords)
      resizeTo(rhs.nWords)
    cforRange(0 until rhs.nWords) { w =>
      words(w) |= rhs.word(w)
    }
    this
  }

  def update(idx: Int, value: Boolean): Unit = {
    val w = idx >>> LogWL
    if (value) {
      if (w >= nWords)
        resizeTo(util.nextPowerOfTwo(w + 1))
      words(w) |= 1L << idx
    } else if (w < nWords) {
      words(w) &= ~(1L << idx)
    }
  }

}

object ResizableBitSet extends mutable.BitSetBuilder[ResizableBitSet] {

  def fromBitmaskNoCopy(words: Array[Long], nWords: Int): ResizableBitSet = new ResizableBitSet(words, nWords)

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

  override def |=(rhs: generic.BitSet): this.type = {
    cforRange(rhs.nWords - 1 to 0 by -1) { w =>
      val word = rhs.word(w)
      if (word != 0L)
        words(w) |= word
    }
    this
  }

  def update(idx: Int, value: Boolean): Unit = {
    val w = idx >>> LogWL
    if (value) {
      words(w) |= 1L << idx
    } else if (w < nWords) {
      words(w) &= ~(1L << idx)
    }
  }

}

object FixedBitSet extends mutable.BitSetBuilder[FixedBitSet] {

  def fromBitmaskNoCopy(words: Array[Long], nWords: Int): FixedBitSet = new FixedBitSet(words)

}
