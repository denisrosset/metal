package net.alasc.ptrcoll
package maps

import scala.annotation.{switch, tailrec}
import scala.{specialized => sp}
import scala.reflect.ClassTag

import spire.algebra.Order
import spire.syntax.cfor._
import spire.util.Opt

/** Mutable hash map where values are pairs (V1, V2). */
class HashMap2[@sp(Int, Long) K, V1, V2](
  /** Slots for keys. */
  var keys: Array[K],
  /** Slots for values, used to store alternatively values of type V1 and V2.
    * Thus vals.length == 2 * keys.length.
    */
  var vals: Array[Any],
  /** Status of the slots in the hash table.
    * 
    * 0 = unused
    * 2 = once used, now empty but not yet overwritten
    * 3 = used
    */ 
  var buckets: Array[Byte],
  /** Number of defined slots. */
  var len: Int,
  /** Number of used slots (used >= len). */
  var used: Int,
  // hashing internals
  /** size - 1, used for hashing. */
  var mask: Int,
  /** Point at which we should grow. */
  var limit: Int)(implicit val ctK: ClassTag[K], val ctV1: ClassTag[V1], val ctV2: ClassTag[V2]) extends MMap2[K, V1, V2] {

  final def size: Int = len

  final override def isEmpty: Boolean = len == 0

  final override def nonEmpty: Boolean = len > 0

  def copy: HashMap2[K, V1, V2] = new HashMap2[K, V1, V2](
    keys = keys.clone,
    vals = vals.clone,
    buckets = buckets.clone,
    len = len,
    used = used,
    mask = mask,
    limit = limit)

  final def update(key: K, value1: V1, value2: V2): Unit = {
    @inline @tailrec def loop(i: Int, perturbation: Int): Unit = {
      val j = i & mask
      val status = buckets(j)
      if (status == 0) {
        keys(j) = key
        vals(2*j) = value1
        vals(2*j + 1) = value2
        buckets(j) = 3
        len += 1
        used += 1
        if (used > limit) grow()
      } else if (status == 2 && !contains(key)) {
        keys(j) = key
        vals(2*j) = value1
        vals(2*j + 1) = value2
        buckets(j) = 3
        len += 1
      } else if (keys(j) == key) {
        vals(2*j) = value1
        vals(2*j + 1) = value2
      } else {
        loop((i << 2) + i + perturbation + 1, perturbation >> 5)
      }
    }
    val i = key.## & 0x7fffffff
    loop(i, i)
  }

  @inline final def remove(key: K): Boolean = ptrFind(key) match {
    case VPtr(vp) =>
      ptrRemove(vp)
      true
    case _ => false
  }

  @inline final def ptrRemoveAndAdvance(ptr: VPtr[Tag]): Ptr[Tag] = {
    val next = ptrNext(ptr)
    ptrRemove(ptr)
    next
  }

  @inline final def -=(key: K): this.type = { remove(key); this }

  @inline final def ptrRemove(ptr: VPtr[Tag]): Unit = {
    val j = ptr.v.toInt
    buckets(j) = 2
    vals(2*j) = null
    vals(2*j + 1) = null
    len -= 1
  }

  @inline final def contains(key: K): Boolean = ptrFind(key).nonNull

  @inline final def ptrFind(key: K): Ptr[Tag] = {
    @inline @tailrec def loop(i: Int, perturbation: Int): Ptr[Tag] = {
      val j = i & mask
      val status = buckets(j)
      if (status == 0) Ptr.Null[Tag]
      else if (status == 3 && keys(j) == key) VPtr[Tag](j)
      else loop((i << 2) + i + perturbation + 1, perturbation >> 5)
    }
    val i = key.## & 0x7fffffff
    loop(i, i)
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
  private[this] def absorb(rhs: HashMap2[K, V1, V2]): Unit = {
    keys = rhs.keys
    vals = rhs.vals
    buckets = rhs.buckets
    len = rhs.len
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
  final def grow(): Dummy[K] = {
    val next = keys.length * (if (keys.length < 10000) 4 else 2)
    val map = HashMap2.ofSize[K, V1, V2](next)
    cfor(0)(_ < buckets.length, _ + 1) { i =>
      if (buckets(i) == 3) {
        val v1 = vals(2*i).asInstanceOf[V1]
        val v2 = vals(2*i+1).asInstanceOf[V2]
        map.update(keys(i), v1, v2)
      }
    }
    absorb(map)
    null
  }

  @inline final def ptrStart: Ptr[Tag] = {
    var i = 0
    while (i < buckets.length && buckets(i) != 3) i += 1
    if (i < buckets.length) VPtr[Tag](i) else Ptr.Null[Tag]
  }

  @inline final def ptrNext(ptr: VPtr[Tag]): Ptr[Tag] = {
    var i = ptr.v.toInt + 1
    while (i < buckets.length && buckets(i) != 3) i += 1
    if (i < buckets.length) VPtr[Tag](i) else Ptr.Null[Tag]
  }

  @inline final def ptrKey(ptr: VPtr[Tag]): K = keys(ptr.v.toInt)

  @inline final def ptrValue1(ptr: VPtr[Tag]): V1 = vals(ptr.v.toInt*2).asInstanceOf[V1]

  @inline final def ptrValue2(ptr: VPtr[Tag]): V2 = vals(ptr.v.toInt*2+1).asInstanceOf[V2]

}

object HashMap2 extends MMap2Factory[Any, Dummy, Any, Any] {

  def empty[@sp(Int, Long) K, V1, V2](implicit ctK: ClassTag[K], d: Dummy[K], e: KLBEv[K], ctV1: ClassTag[V1], ctV2: ClassTag[V2]): HashMap2[K, V1, V2] = ofSize(0)(ctK, d, e, ctV1, ctV2)

  /** Creates a HashMap that can hold n unique keys without resizing itself.
    *
    * Note that the internal representation will allocate more space
    * than requested to satisfy the requirements of internal
    * alignment. Map uses arrays whose lengths are powers of two, and
    * needs at least 33% of the map free to enable good hashing
    * performance.
    * 
    * Example: HashMap.ofSize[Int, String](100).
    */

  def ofSize[@sp(Int, Long) K: ClassTag: Dummy: KLBEv, V1: ClassTag, V2: ClassTag](n: Int): HashMap2[K, V1, V2] = ofAllocatedSize(n / 2 * 3)

  /** Allocates an empty HashMap, with underlying storage of size n.
    * 
    * This method is useful if you know exactly how big you want the
    * underlying array to be. In most cases ofSize() is probably what
    * you want instead.
    */
  private[ptrcoll] def ofAllocatedSize[@sp(Int, Long) K: ClassTag, V1: ClassTag, V2: ClassTag](n: Int) = {
    val sz = Util.nextPowerOfTwo(n) match {
      case n if n < 0 => throw PtrCollOverflowError(n)
      case 0 => 8
      case n => n
    }
    new HashMap2[K, V1, V2](
      keys = new Array[K](sz),
      vals = new Array[Any](sz*2),
      buckets = new Array[Byte](sz),
      len = 0,
      used = 0,
      mask = sz - 1,
      limit = (sz * 0.65).toInt)
  }

}
