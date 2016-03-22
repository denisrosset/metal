package metal
package generic

import scala.annotation.tailrec

import util.Dummy

abstract class HashMap[K, V] extends generic.Map[K, V] {

  import HashMap.{UNUSED, USED}

  type Immutable = immutable.HashMap[K, V]
  type Mutable = mutable.HashMap[K, V]

  /** Number of defined slots. */
  def size: Int
  @inline final def longSize = size

  /** Number of used slots. */
  def used: Int

  /** Mask = nSlots - 1, used for hashing. */
  def mask: Int

  /** Point at which we should grow. */
  def limit: Int

  def nSlots: Int = buckets.length
  def key(i: Int): K = key(i)
  def value(i: Int): V = value(i)
  def bucket(i: Int): Byte = buckets(i)

  /** Status of the slots in the hash table. */
  private[metal] def buckets: Array[Byte]

  /** Slots for keys. */
  private[metal] def keys: Array[K]

  /** Slots for values. */
  private[metal] def values: Array[V]

  def mutableCopy = new mutable.HashMap[K, V](keys.clone, buckets.clone, values.clone, size, used, mask, limit)

  @inline final def isEmpty = size == 0
  @inline final def nonEmpty = size > 0

  def keyArray(ptr: VPtr[this.type]): Array[K] = keys
  def keyIndex(ptr: VPtr[this.type]): Int = ptr.raw.toInt

  def valueArray(ptr: VPtr[this.type]): Array[V] = values
  def valueIndex(ptr: VPtr[this.type]): Int = ptr.raw.toInt

  final def ptrFind[@specialized L](key: L): Ptr[this.type] = {
    val keysL = keys.asInstanceOf[Array[L]]
    @inline @tailrec def loop(i: Int, perturbation: Int): Ptr[this.type] = {
      val j = i & mask
      val status = buckets(j)
      if (status == UNUSED) Ptr.Null(this)
      else if (status == USED && keysL(j) == key) VPtr(this, j)
      else loop((i << 2) + i + perturbation + 1, perturbation >> 5)
    }
    val i = K.asInstanceOf[Methods[L]].hash(key) & 0x7fffffff
    loop(i, i)
  }

  final def ptr: Ptr[this.type] = {
    var i = 0
    while (i < buckets.length && buckets(i) != 3) i += 1
    if (i < buckets.length) VPtr(this, i) else Ptr.Null(this)
  }

  final def ptrNext(ptr: VPtr[this.type]): Ptr[this.type] = {
    var i = ptr.raw.toInt + 1
    while (i < buckets.length && buckets(i) != 3) i += 1
    if (i < buckets.length) VPtr(this, i) else Ptr.Null(this)
  }

  final def ptrKey[@specialized L](ptr: VPtr[this.type]): L = keys.asInstanceOf[Array[L]](ptr.raw.toInt)

  final def ptrValue[@specialized W](ptr: VPtr[this.type]): W = values.asInstanceOf[Array[W]](ptr.raw.toInt)

  final def ptrElement1[@specialized E1](ptr: VPtr[this.type]): E1 = keys.asInstanceOf[Array[E1]](ptr.raw.toInt)

  final def ptrElement2[@specialized E2](ptr: VPtr[this.type]): E2 = values.asInstanceOf[Array[E2]](ptr.raw.toInt)

}

object HashMap {

  /** Unused bucket. */
  @inline final def UNUSED: Byte = 0

  /** Once used, has been deleted but not yet overwritten. */
  @inline final def DELETED: Byte = 2

  /** Used. */
  @inline final def USED: Byte = 3

}

trait HashMapFactory extends MapFactory {

  type KExtra[K] = Dummy[K]
  type VExtra[V] = Dummy[V]

  type M[K, V] <: generic.HashMap[K, V]

}
