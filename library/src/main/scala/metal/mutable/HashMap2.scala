package metal
package mutable

import scala.annotation.tailrec
import spire.math.max
import spire.syntax.cfor._

import generic.Methods

final class HashMap2[K, V1, V2](
  var keys: Array[K],
  var buckets: Array[Byte],
  var values1: Array[V1],
  var values2: Array[V2],
  var size: Int,
  var used: Int,
  var mask: Int,
  var limit: Int)(implicit val K: Methods[K], val V1: Methods[V1], val V2: Methods[V2])
    extends generic.HashMap2[K, V1, V2] with mutable.Map2[K, V1, V2] {

  import generic.HashMap2.{UNUSED, DELETED, USED}

  def toImmutable = new immutable.HashMap2[K, V1, V2](keys, buckets, values1, values2, size, used, mask, limit)

  def reset(): Unit = {
    cforRange(0 until nSlots) { i =>
      buckets(i) = UNUSED
      keys(i) = null.asInstanceOf[K]
      values1(i) = null.asInstanceOf[V1]
      values2(i) = null.asInstanceOf[V2]
    }
    size = 0
    used = 0
  }

  def result() = {
    val res = new immutable.HashMap2[K, V1, V2](keys, buckets, values1, values2, size, used, mask, limit)
    buckets = new Array[Byte](8) // optimize using empty array
    keys = K.newArray(8)
    values1 = V1.newArray(8)
    values2 = V2.newArray(8)
    size = 0
    used = 0
    mask = 8 - 1
    limit = (8 * 0.65).toInt
    res
  }

  def clear(): Unit = {
    keys = K.newArray(8)
    buckets = new Array[Byte](8)
    values1 = V1.newArray(8)
    values2 = V2.newArray(8)
    size = 0
    used = 0
    mask = 8 - 1
    limit = (8 * 0.65).toInt
  }

  final def ptrAddKey[@specialized L](key: L): VPtr[this.type] = {
    val keysL = keys.asInstanceOf[Array[L]]
    // iteration loop, `i` is the current probe, `perturbation` is used to compute the
    // next probe, and `freeBlock` is the first STATUS_DELETED bucket in the sequence
    @inline @tailrec def loop(i: Int, perturbation: Int, freeBlock: Int): VPtr[this.type] = {
      val j = i & mask
      val status = buckets(j)
      if (status == UNUSED) {
        val writeTo = if (freeBlock == -1) j else freeBlock
        keysL(writeTo) = key
        val oldStatus = buckets(writeTo)
        buckets(writeTo) = USED
        size += 1
        if (oldStatus == DELETED) // we reuse a bucket
          VPtr(this, writeTo)
        else { // new bucket occupied
          used += 1
          if (used > limit) {
            grow()
            val IsVPtr(vp) = ptrFind[L](key)
            vp
          } else VPtr(this, writeTo)
        }
      } else if (status == DELETED) {
        val newFreeBlock = if (freeBlock == -1) j else freeBlock
        loop((i << 2) + i + perturbation + 1, perturbation >> 5, newFreeBlock)
      } else if (status == USED && keysL(j) == key) {
        VPtr(this, j)
      } else {
        loop((i << 2) + i + perturbation + 1, perturbation >> 5, freeBlock)
      }
    }
    val i = K.asInstanceOf[Methods[L]].hash(key) & 0x7fffffff
    loop(i, i, -1)
  }

  final def ptrUpdate1[@specialized W1](vp: VPtr[this.type], v1: W1): Unit = {
    values1.asInstanceOf[Array[W1]](vp.raw.toInt) = v1
  }

  final def ptrUpdate2[@specialized W2](vp: VPtr[this.type], v2: W2): Unit = {
    values2.asInstanceOf[Array[W2]](vp.raw.toInt) = v2
  }

  final def ptrRemoveAndAdvance(vp: VPtr[this.type]): Ptr[this.type] = {
    val next = ptrNext(vp)
    ptrRemove(vp)
    next
  }

  final def ptrRemove(vp: VPtr[this.type]): Unit = {
    val j = vp.raw.toInt
    buckets(j) = DELETED
    keys(j) = null.asInstanceOf[K]
    values1(j) = null.asInstanceOf[V1]
    values2(j) = null.asInstanceOf[V2]
    size -= 1
  }

  /** Absorbs the given map's contents into this map.
    * 
    * This method does not copy the other map's contents. Thus, this
    * should only be used when there are no saved references to the
    * other map. It is private, and exists primarily to simplify the
    * implementation of certain methods.
    * 
    * This is an O(1) operation, although it can potentially generate a
    * lot of garbage (if the map was previously large).
    */
  private[this] def absorb(rhs: mutable.HashMap2[K, V1, V2]): Unit = {
    keys = rhs.keys
    values1 = rhs.values1
    values2 = rhs.values2
    buckets = rhs.buckets
    size = rhs.size
    used = rhs.used
    mask = rhs.mask
    limit = rhs.limit
  }

  /**
    * Grow the underlying array to best accomodate the map's size.
    * 
    * To preserve hashing access speed, the map's size should never be
    * more than 66% of the underlying array's size. When this size is
    * reached, the map needs to be updated (using this method) to have a
    * larger array.
    * 
    * The underlying array's size must always be a multiple of 2, which
    * means this method grows the array's size by 2x (or 4x if the map
    * is very small). This doubling helps amortize the cost of
    * resizing, since as the map gets larger growth will happen less
    * frequently. This method returns a null of type Unit1[A] to
    * trigger specialization without allocating an actual instance.
    * 
    * Growing is an O(n) operation, where n is the map's size.
    */
  final def grow(): Unit = {
    val next = keys.length * (if (keys.length < 10000) 4 else 2)
    val map = mutable.HashMap2.ofAllocatedSize[K, V1, V2](next)
    cfor(0)(_ < buckets.length, _ + 1) { i =>
      if (buckets(i) == 3) {
        val vp = map.ptrAddKeyFromArray(keys, i)
        map.ptrUpdate1FromArray(vp, values1, i)
        map.ptrUpdate2FromArray(vp, values2, i)
      }
    }
    absorb(map)
  }

}

object HashMap2 extends generic.HashMap2Factory with mutable.Map2Factory {

  type M[K, V1, V2] = mutable.HashMap2[K, V1, V2]

  /** Allocates an empty HashMapImpl, with underlying storage of size n.
    * 
    * This method is useful if you know exactly how big you want the
    * underlying array to be. In most cases reservedSize() is probably what
    * you want instead.
    */
  private[metal] def ofAllocatedSize[K, V1, V2](n: Int)(implicit K: Methods[K], V1: Methods[V1], V2: Methods[V2]) = {
    import K.{classTag => ctK}
    import V1.{classTag => ctV1}
    import V2.{classTag => ctV2}
    val sz = util.nextPowerOfTwo(n) match {
      case n if n < 0 => sys.error(s"Bad allocated size $n for collection")
      case 0 => 8
      case n => n
    }
    new mutable.HashMap2[K, V1, V2](
      keys = K.newArray(sz),
      buckets = new Array[Byte](sz),
      values1 = V1.newArray(sz),
      values2 = V2.newArray(sz),
      size = 0,
      used = 0,
      mask = sz - 1,
      limit = (sz * 0.65).toInt)
  }

  def reservedSize[K:Methods:KExtra, V1:Methods:V1Extra, V2:Methods:V2Extra](n: Int): M[K, V1, V2] = ofAllocatedSize[K, V1, V2](max(n / 2 * 3, n))

}
