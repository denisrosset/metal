package metal
package impl

import scala.annotation.{switch, tailrec}
import scala.reflect.ClassTag

import spire.algebra.Order
import spire.syntax.cfor._
import spire.util.Opt

class HashMapImpl[K, V](
  /** Slots for keys. */
  var keys: Array[K],
  /** Slots for values. */
  var values: Array[V],
  /** Status of the slots in the hash table.
    * 
    * 0 = unused
    * 2 = once used, now empty but not yet overwritten
    * 3 = used
    */ 
  var buckets: Array[Byte],
  /** Number of defined slots = buckets.count(_ > 1). */
  var len: Int,
  /** Number of used slots (used >= len). */
  var used: Int,
  // hashing internals
  /** size - 1, used for hashing. */
  var mask: Int,
  /** Point at which we should grow. */
  var limit: Int
)(implicit val K: Methods[K], val V: Methods[V]) extends IHashMap[K, V] with MHashMap[K, V] {

  @inline final def STATUS_UNUSED: Int = 0
  @inline final def STATUS_DELETED: Int = 2
  @inline final def STATUS_USED: Int = 3

  // Map implementation

  type Cap = Nextable with Removable with Keys[K] with Searchable[K] with Values[V] with Updatable[V]  with Elements2[K, V]
  @inline final def longSize: Long = len

  @inline final override def isEmpty: Boolean = len == 0

  @inline final override def nonEmpty: Boolean = len > 0

  def keyArray(ptr: MyVPtr): Array[K] = keys
  def keyIndex(ptr: MyVPtr): Int = ptr.raw.toInt
  def valueArray(ptr: MyVPtr): Array[V] = values
  def valueIndex(ptr: MyVPtr): Int = ptr.raw.toInt

  def result(): IHashMap[K, V] = this

  def mutableCopy(): MHashMap[K, V] = new HashMapImpl[K, V](
    keys = keys.clone,
    values = values.clone,
    buckets = buckets.clone,
    len = len,
    used = used,
    mask = mask,
    limit = limit)

  def clear(): Unit = {
    absorb(MHashMap.empty[K, V])
  }

  final def ptrAddKey[@specialized L](key: L): MyVPtr = {
    val keysL = keys.asInstanceOf[Array[L]]
    // iteration loop, `i` is the current probe, `perturbation` is used to compute the
    // next probe, and `freeBlock` is the first STATUS_DELETED bucket in the sequence
    @inline @tailrec def loop(i: Int, perturbation: Int, freeBlock: Int): MyVPtr = {
      val j = i & mask
      val status = buckets(j)
      if (status == STATUS_UNUSED) {
        val writeTo = if (freeBlock == -1) j else freeBlock
        keysL(writeTo) = key
        val oldStatus = buckets(writeTo)
        buckets(writeTo) = 3
        len += 1
        if (oldStatus == STATUS_DELETED) // we reuse a bucket
          VPtr(this, writeTo)
        else { // new bucket occupied
          used += 1
          if (used > limit) {
            grow()
            val IsVPtr(vp) = ptrFind[L](key)
            vp
          } else VPtr(this, writeTo)
        }
      } else if (status == STATUS_DELETED) {
        val newFreeBlock = if (freeBlock == -1) j else freeBlock
        loop((i << 2) + i + perturbation + 1, perturbation >> 5, newFreeBlock)
      } else if (status == STATUS_USED && keysL(j) == key) {
        VPtr(this, j)
      } else {
        loop((i << 2) + i + perturbation + 1, perturbation >> 5, freeBlock)
      }
    }
    val i = K.asInstanceOf[Methods[L]].hash(key) & 0x7fffffff
    loop(i, i, -1)
  }

  final def ptrUpdate[@specialized W](vp: MyVPtr, v: W): Unit = {
    values.asInstanceOf[Array[W]](vp.raw.toInt) = v
  }


  final def ptrRemoveAndAdvance(vp: MyVPtr): MyPtr = {
    val next = ptrNext(vp)
    ptrRemove(vp)
    next
  }

  final def ptrRemove(vp: MyVPtr): Unit = {
    val j = vp.raw.toInt
    buckets(j) = 2
    keys(j) = null.asInstanceOf[K]
    values(j) = null.asInstanceOf[V]
    len -= 1
  }

  final def ptrFind[@specialized L](key: L): MyPtr = {
    val keysL = keys.asInstanceOf[Array[L]]
    @inline @tailrec def loop(i: Int, perturbation: Int): MyPtr = {
      val j = i & mask
      val status = buckets(j)
      if (status == STATUS_UNUSED) Ptr.`null`(this)
      else if (status == STATUS_USED && keysL(j) == key) VPtr(this, j)
      else loop((i << 2) + i + perturbation + 1, perturbation >> 5)
    }
    val i = K.asInstanceOf[Methods[L]].hash(key) & 0x7fffffff
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
  private[this] def absorb(rhs: MHashMap[K, V]): Unit = {
    keys = rhs.keys
    values = rhs.values
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
  final def grow(): Unit = {
    val next = keys.length * (if (keys.length < 10000) 4 else 2)
    val map = MHashMap.ofSize[K, V](next)
    cfor(0)(_ < buckets.length, _ + 1) { i =>
      if (buckets(i) == 3) {
        val vp = map.ptrAddKeyFromArray(keys, i)
        map.ptrUpdateFromArray(vp, values, i)
      }
    }
    absorb(map)
  }

  final def ptr: MyPtr = {
    var i = 0
    while (i < buckets.length && buckets(i) != 3) i += 1
    if (i < buckets.length) VPtr(this, i) else Ptr.`null`(this)
  }

  final def ptrNext(ptr: MyVPtr): MyPtr = {
    var i = ptr.raw.toInt + 1
    while (i < buckets.length && buckets(i) != 3) i += 1
    if (i < buckets.length) VPtr(this, i) else Ptr.`null`(this)
  }

  final def ptrKey[@specialized L](ptr: MyVPtr): L = keys.asInstanceOf[Array[L]](ptr.raw.toInt)

  final def ptrValue[@specialized W](ptr: MyVPtr): W = values.asInstanceOf[Array[W]](ptr.raw.toInt)

  final def ptrElement1[@specialized E1](ptr: MyVPtr): E1 = keys.asInstanceOf[Array[E1]](ptr.raw.toInt)

  final def ptrElement2[@specialized E2](ptr: MyVPtr): E2 = values.asInstanceOf[Array[E2]](ptr.raw.toInt)

}

object HashMapImpl {

  /** Allocates an empty HashMapImpl, with underlying storage of size n.
    * 
    * This method is useful if you know exactly how big you want the
    * underlying array to be. In most cases ofSize() is probably what
    * you want instead.
    */
  private[metal] def ofAllocatedSize[K, V](n: Int)(implicit K: Methods[K], V: Methods[V]) = {
    import K.{classTag => ctK}
    import V.{classTag => ctV}
    val sz = Util.nextPowerOfTwo(n) match {
      case n if n < 0 => sys.error(s"Bad allocated size $n for collection")
      case 0 => 8
      case n => n
    }
    new HashMapImpl[K, V](
      keys = K.newArray(sz),
      values = V.newArray(sz),
      buckets = new Array[Byte](sz),
      len = 0,
      used = 0,
      mask = sz - 1,
      limit = (sz * 0.65).toInt)
  }

}
