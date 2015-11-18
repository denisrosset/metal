package metal
package impl

final class BitSetImpl(var words: Array[Long], var wordSize: Int) extends IBitSet[Int] with MBitSet[Int] {

  override def priorityEquals = true

  override def ptrHash(ptr: MyVPtr): Int = ptr.raw.toInt

  override def ptrToString(ptr: MyVPtr): String = ptr.raw.toInt.toString

  override def ptrEquals(thisPtr: MyVPtr, that: FSet[Int]): Boolean =
    that.ptrFind[Int](thisPtr.raw.toInt).nonNull

  def keyArray(ptr: MyVPtr) = sys.error("Cannot call keyArray on BitSet")
  def keyIndex(ptr: MyVPtr) = sys.error("Cannot call keyArray on BitSet")

  def order = spire.std.int.IntAlgebra

  def K = Methods.Int

  def mutableCopy(): MBitSet[Int] = new BitSetImpl(words.clone, wordSize)

  def result(): IBitSet[Int] = this

  def ptr: MyPtr = {
    var w = 0
    while(w < wordSize && words(w) == 0L) {
      w += 1
    }
    if (w == wordSize) return Ptr.`null`(this)
    val index = w * 8 + java.lang.Long.numberOfTrailingZeros(words(w))
    Ptr(this, index)
  }

  def ptrFind[@specialized L](keyL: L): MyPtr = {
    val key = keyL.asInstanceOf[Int]
    val w = key >>> 3
    val bit = key & 0x7
    val contained = w < wordSize && (words(w) & (1 << bit)) != 0
    if (contained) Ptr(this, key) else Ptr.`null`(this)
  }

  def ptrNext(ptr: MyVPtr): MyPtr = {
    var w = ptr.raw.toInt >>> 3
    var bit = (ptr.raw & 0x7).toInt
    val nextBit = Util.nextBitAfter(words(w), bit)
    if (nextBit >= 0) return Ptr(this, ptr.raw - bit + nextBit)
    w += 1
    if (w == wordSize) return Ptr.`null`(this)
    while(w < wordSize && words(w) == 0L) {
      w += 1
    }
    if (w == wordSize) return Ptr.`null`(this)
    val index = w * 8 + java.lang.Long.numberOfTrailingZeros(words(w))
    Ptr(this, index)
  }

  def ptrKey[@specialized L](ptr: MyVPtr): L = ptr.raw.toInt.asInstanceOf[L]

  def ptrElement[@specialized E](ptr: MyVPtr): E = ptr.raw.toInt.asInstanceOf[E]

  def ptrAddKey[@specialized L](keyL: L): MyVPtr = {
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
    VPtr(this, key)
  }

  def ptrRemove(ptr: MyVPtr): Unit = {
    val w = ptr.raw.toInt >>> 3
    val bit = ptr.raw & 0x7
    if (w >= wordSize) return
    val masked = words(w) & (1 << bit)
    words(w) -= masked
  }

  def ptrRemoveAndAdvance(ptr: MyVPtr): MyPtr = {
    val nextPtr = ptrNext(ptr)
    ptrRemove(ptr)
    nextPtr
  }

  def longSize: Long = {
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
