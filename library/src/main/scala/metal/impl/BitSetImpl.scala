package metal
package impl

final class BitSetImpl(var words: Array[Long], var wordSize: Int) extends IBitSet[Int] with MBitSet[Int] {

  override def priorityEquals = true

  override def ptrHash(ptr: VPtr[Tag]): Int = ptr.v.toInt

  override def ptrToString(ptr: VPtr[Tag]): String = ptr.v.toInt.toString

  override def ptrEquals(thisPtr: VPtr[Tag], that: FSet[Int]): Boolean =
    that.ptrFind[Int](thisPtr.v.toInt).nonNull

  def keyArray(ptr: VPtr[Tag]) = sys.error("Cannot call keyArray on BitSet")
  def keyIndex(ptr: VPtr[Tag]) = sys.error("Cannot call keyArray on BitSet")

  def order = spire.std.int.IntAlgebra

  def K = Methods.Int

  def mutableCopy(): MBitSet[Int] = new BitSetImpl(words.clone, wordSize)

  def result(): IBitSet[Int] = this

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
    wordSize = scala.math.max(wordSize, w + 1)
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

  def size: Long = {
    var count = 0L
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
