package metal
package mutable

import scala.annotation.tailrec
import scala.reflect.ClassTag

import spire.math.max
import spire.syntax.cfor._

final class HashMap[K, V](
  var keys: Array[K],
  var buckets: Array[Byte],
  var values: Array[V],
  var size: Int,
  var used: Int,
  var mask: Int,
  var limit: Int)(implicit
    val ctK: ClassTag[K],
    val K: MetalTag[K],
    val ctV: ClassTag[V],
    val V: MetalTag[V])
    extends generic.HashMap[K, V]
    with mutable.Map[K, V] {

  import generic.HashMap.{UNUSED, DELETED, USED}

  def toImmutable = new immutable.HashMap[K, V](keys, buckets, values, size, used, mask, limit)

  def reset(): Unit = {
    cforRange(0 until nSlots) { i =>
      buckets(i) = UNUSED
      keys(i) = null.asInstanceOf[K]
      values(i) = null.asInstanceOf[V]
    }
    size = 0
    used = 0
  }

  def result() = {
    val res = new immutable.HashMap[K, V](keys, buckets, values, size, used, mask, limit)
    buckets = new Array[Byte](8) // optimize using empty array
    keys = ctK.newArray(8)
    values = ctV.newArray(8)
    size = 0
    used = 0
    mask = 8 - 1
    limit = (8 * 0.65).toInt
    res
  }

  def clear(): Unit = {
    keys = ctK.newArray(8)
    buckets = new Array[Byte](8)
    values = ctV.newArray(8)
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
    val i = K.asInstanceOf[MetalTag[L]].hash(key) & 0x7fffffff
    loop(i, i, -1)
  }

  final def ptrUpdate[@specialized W](vp: VPtr[this.type], v: W): Unit = {
    values.asInstanceOf[Array[W]](vp.raw.toInt) = v
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
    values(j) = null.asInstanceOf[V]
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
  private[this] def absorb(rhs: mutable.HashMap[K, V]): Unit = {
    keys = rhs.keys
    values = rhs.values
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
    val map = mutable.HashMap.ofAllocatedSize[K, V](next)
    cfor(0)(_ < buckets.length, _ + 1) { i =>
      if (buckets(i) == 3) {
        val vp = map.ptrAddKeyFromArray(keys, i)
        map.ptrUpdateFromArray(vp, values, i)
      }
    }
    absorb(map)
  }

}

object HashMap extends generic.HashMapFactory with mutable.MapFactory {

  type M[K, V] = mutable.HashMap[K, V]

  /** Allocates an empty HashMapImpl, with underlying storage of size n.
    * 
    * This method is useful if you know exactly how big you want the
    * underlying array to be. In most cases reservedSize() is probably what
    * you want instead.
    */
  private[metal] def ofAllocatedSize[K, V](n: Int)(implicit K: ClassTag[K], V: ClassTag[V]) = {
    val sz = util.nextPowerOfTwo(n) match {
      case n if n < 0 => sys.error(s"Bad allocated size $n for collection")
      case 0 => 8
      case n => n
    }
    new mutable.HashMap[K, V](
      keys = K.newArray(sz),
      buckets = new Array[Byte](sz),
      values = V.newArray(sz),
      size = 0,
      used = 0,
      mask = sz - 1,
      limit = (sz * 0.65).toInt)
  }

  def reservedSize[K:ClassTag:KExtra, V:ClassTag:VExtra](n: Int): M[K, V] = ofAllocatedSize[K, V](max(n / 2 * 3, n))

}
