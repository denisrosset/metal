package metal
package mutable

import scala.annotation.tailrec
import scala.reflect.ClassTag

import spire.math.max
import spire.syntax.cfor._

final class HashSet[K](
  var keys: Array[K],
  var buckets: Array[Byte],
  var size: Int,
  var used: Int,
  var mask: Int,
  var limit: Int)(implicit
    val ctK: ClassTag[K],
    val K: MetalTag[K]
) extends generic.HashSet[K] with mutable.Set[K] {

  import generic.HashSet.{UNUSED, DELETED, USED}

  def reset(): Unit = {
    cforRange(0 until nSlots) { i =>
      buckets(i) = UNUSED
      keys(i) = null.asInstanceOf[K]
    }
    size = 0
    used = 0
  }

  def clear(): Unit = {
    keys = ctK.newArray(8)
    buckets = new Array[Byte](8)
    size = 0
    used = 0
    mask = 8 - 1
    limit = (8 * 0.65).toInt
  }

  def toImmutable = new immutable.HashSet[K](keys.clone, buckets.clone, size, used, mask, limit) // TODO: trim arrays?

  def result() = {
    val res = new immutable.HashSet[K](keys, buckets, size, used, mask, limit)
    buckets = new Array[Byte](8) // optimize using empty array
    keys = ctK.newArray(8)
    size = 0
    used = 0
    mask = 8 - 1
    limit = (8 * 0.65).toInt
    res
  }
  
  def ptrRemoveAndAdvance(ptr: VPtr[this.type]): Ptr[this.type] = {
    val next = ptrNext(ptr)
    ptrRemove(ptr)
    next
  }

  def ptrRemove(ptr: VPtr[this.type]): Unit = {
    val j = ptr.raw.toInt
    buckets(j) = DELETED
    keys(j) = null.asInstanceOf[K]
    size -= 1
  }

  def ptrAddKey[@specialized L](key: L): VPtr[this.type] = {
    val keysL = keys.asInstanceOf[Array[L]]
    // iteration loop, `i` is the current probe, `perturbation` is used to compute the
    // next probe, and `freeBlock` is the first DELETED bucket in the sequence
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

  /**
    * Grow the underlying array to best accomodate the set's size.
    * 
    * To preserve hashing access speed, the set's size should never be
    * more than 66% of the underlying array's size. When this size is
    * reached, the set needs to be updated (using this method) to have a
    * larger array.
    * 
    * The underlying array's size must always be a multiple of 2, which
    * means this method grows the array's size by 2x (or 4x if the set
    * is very small). This doubling helps amortize the cost of
    * resizing, since as the set gets larger growth will happen less
    * frequently. This method returns a null of type Unit1[A] to
    * trigger specialization without allocating an actual instance.
    * 
    * Growing is an O(n) operation, where n is the set's size.
    */
  def grow(): Unit = {
    val next = buckets.length * (if (buckets.length < 10000) 4 else 2)
    val set = mutable.HashSet.ofAllocatedSize[K](next)
    cfor(0)(_ < buckets.length, _ + 1) { i =>
      if (buckets(i) == 3) {
        set.ptrAddKeyFromArray(keys, i)
      }
    }
    absorb(set)
  }

  /**
    * Absorb the given set's contents into this set.
    * 
    * This method does not copy the other set's contents. Thus, this
    * should only be used when there are no saved references to the
    * other set. It is private, and exists primarily to simplify the
    * implementation of certain methods.
    * 
    * This is an O(1) operation, although it can potentially generate a
    * lot of garbage (if the set was previously large).
    */
  private[this] def absorb(that: mutable.HashSet[K]): Unit = {
    keys = that.keys
    buckets = that.buckets
    size = that.size
    used = that.used
    mask = that.mask
    limit = that.limit
  }


}

object HashSet extends generic.HashSetFactory with mutable.SetFactory {

  type S[K] = mutable.HashSet[K]

  /**
    * Allocate an empty HashSet, with underlying storage of size n.
    * 
    * This method is useful if you know exactly how big you want the
    * underlying array to be. In most cases reservedSize() is probably what
    * you want instead.
    */
  def ofAllocatedSize[K](n: Int)(implicit K: ClassTag[K]): S[K] = {
    val sz = util.nextPowerOfTwo(n) match {
      case n if n < 0 => sys.error(s"Bad allocated size $n for collection")
      case 0 => 8
      case n => n
    }
    new mutable.HashSet[K](
      keys = K.newArray(sz),
      buckets = new Array[Byte](sz),
      size = 0,
      used = 0,
      mask = sz - 1,
      limit = (sz * 0.65).toInt)
  }

  def reservedSize[K:ClassTag:Extra](n: Int): S[K] = ofAllocatedSize(max(n / 2 * 3, n))

}
