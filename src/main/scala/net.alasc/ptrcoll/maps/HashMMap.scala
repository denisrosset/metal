package net.alasc.ptrcoll
package maps

import scala.annotation.{switch, tailrec}
import scala.{specialized => sp}
import scala.reflect.ClassTag

import spire.algebra.Order
import spire.syntax.cfor._
import spire.util.Opt

import syntax.all._

trait HashMMap[@sp(Int, Long) K, V] extends MMap[K, V] {
  protected[ptrcoll] def keys: Array[K]
  protected[ptrcoll] def vals: Array[V]
  protected[ptrcoll] def buckets: Array[Byte]
  protected[ptrcoll] def len: Int
  protected[ptrcoll] def used: Int
  protected[ptrcoll] def mask: Int
  protected[ptrcoll] def limit: Int
}

trait HashMMapImpl[@sp(Int, Long) K, V] extends HashMMap[K, V] with HasPtrAt[K, RawPtr] with HasPtrVal[V, RawPtr] { self =>
  /** Slots for keys. */
  var keys: Array[K]
  /** Slots for values. */
  var vals: Array[V]
  /** Status of the slots in the hash table.
    * 
    * 0 = unused
    * 2 = once used, now empty but not yet overwritten
    * 3 = used
    */ 
  var buckets: Array[Byte]
  /** Number of defined slots. */
  var len: Int
  /** Number of used slots (used >= len). */
  var used: Int

  // hashing internals
  /** size - 1, used for hashing. */
  var mask: Int
  /** Point at which we should grow. */
  var limit: Int

  final def size: Int = len
  final override def isEmpty: Boolean = len == 0
  final override def nonEmpty: Boolean = len > 0

  final def update(key: K, value: V): Unit = {
    @inline @tailrec def loop(i: Int, perturbation: Int): Unit = {
      val j = i & mask
      val status = buckets(j)
      if (status == 0) {
        keys(j) = key
        vals(j) = value
        buckets(j) = 3
        len += 1
        used += 1
        if (used > limit) grow()
      } else if (status == 2 && !contains(key)) {
        keys(j) = key
        vals(j) = value
        buckets(j) = 3
        len += 1
      } else if (keys(j) == key) {
        vals(j) = value
      } else {
        loop((i << 2) + i + perturbation + 1, perturbation >> 5)
      }
    }
    val i = key.## & 0x7fffffff
    loop(i, i)
  }

  final def removeAt(ptr: ValidPtr): Unit = {
    val j = ptr.toInt
    buckets(j) = 2
    vals(j) = null.asInstanceOf[V]
    len -= 1
  }

  def findPointerAt(key: K): Ptr = {
    @inline @tailrec def loop(i: Int, perturbation: Int): Ptr = {
      val j = i & mask
      val status = buckets(j)
      if (status == 0) nullPtr
      else if (status == 3 && keys(j) == key) Ptr(j)
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
  private[this] def absorb(rhs: HashMMap[K, V]): Unit = {
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
  final def grow(): Dummy2[K, V] = {
    val next = keys.length * (if (keys.length < 10000) 4 else 2)
    val map = HashMMap.ofSize[K, V](next)
    cfor(0)(_ < buckets.length, _ + 1) { i =>
      if (buckets(i) == 3) map(keys(i)) = vals(i)
    }
    absorb(map)
    null
  }

  @inline final def nullPtr: Ptr = Ptr(-1L)
  def pointer: Ptr = {
    var i = 0
    while (i < buckets.length && buckets(i) != 3) i += 1
    if (i < buckets.length) Ptr(i) else nullPtr
  }

  // PtrTC implementation
  def next(ptr: RawPtr): RawPtr = {
    var i = ptr.toInt + 1
    while (i < buckets.length && buckets(i) != 3) i += 1
    if (i < buckets.length) Ptr(i) else nullPtr
  }
  def hasAt(ptr: RawPtr): Boolean = ptr != -1
  def at(ptr: RawPtr): K = keys(ptr.toInt)
  def atVal(ptr: RawPtr): V = vals(ptr.toInt)

  implicit def PtrTC: HasPtrAt[K, Ptr] with HasPtrVal[V, Ptr] = self.asInstanceOf[HasPtrAt[K, Ptr] with HasPtrVal[V, Ptr]]
}

object HashMMap extends MMapFactory[Any, Any, Dummy] {
  def empty[@sp(Int, Long) K, V](implicit ctK: ClassTag[K], d: Dummy[K], e: KLBEv[K], ctV: ClassTag[V]): HashMMap[K, V] = ofSize(0)(ctK, d, e, ctV)

  /** Creates a HashMMap that can hold n unique keys without resizing itself.
    *
    * Note that the internal representation will allocate more space
    * than requested to satisfy the requirements of internal
    * alignment. Map uses arrays whose lengths are powers of two, and
    * needs at least 33% of the map free to enable good hashing
    * performance.
    * 
    * Example: HashMMap.ofSize[Int, String](100).
    */

  def ofSize[@sp(Int, Long) K: ClassTag: Dummy: KLBEv, V: ClassTag](n: Int): HashMMap[K, V] = ofAllocatedSize(n / 2 * 3)

  /** Allocates an empty HashMMap, with underlying storage of size n.
    * 
    * This method is useful if you know exactly how big you want the
    * underlying array to be. In most cases ofSize() is probably what
    * you want instead.
    */
  private[ptrcoll] def ofAllocatedSize[@sp(Int, Long) K: ClassTag, V: ClassTag](n: Int) = {
    val sz = Util.nextPowerOfTwo(n) match {
      case n if n < 0 => throw PtrCollOverflowError(n)
      case 0 => 8
      case n => n
    }
    new HashMMapImpl[K, V] {
      def ctK = implicitly[ClassTag[K]]
      def ctV = implicitly[ClassTag[V]]
      var keys = new Array[K](sz)
      var vals = new Array[V](sz)
      var buckets = new Array[Byte](sz)
      var len = 0
      var used = 0
      var mask = sz - 1
      var limit = (sz * 0.65).toInt
    }
  }
}
