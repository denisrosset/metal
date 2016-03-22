package metal
package generic

import scala.annotation.tailrec

import util.Dummy

abstract class HashSet[K] extends generic.Set[K] {

  import HashSet.{UNUSED, USED}

  type Immutable = immutable.HashSet[K]
  type Mutable = mutable.HashSet[K]

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
  def bucket(i: Int): Byte = buckets(i)

  /** Status of the slots in the hash table. */
  private[metal] def buckets: Array[Byte]

  /** Slots for keys. */
  private[metal] def keys: Array[K]

  def mutableCopy = new mutable.HashSet[K](keys.clone, buckets.clone, size, used, mask, limit)

  @inline final def isEmpty = size == 0
  @inline final def nonEmpty = size > 0

  def keyArray(ptr: VPtr[this.type]): Array[K] = keys
  def keyIndex(ptr: VPtr[this.type]): Int = ptr.raw.toInt

  /**
    * Return whether the item is found in the HashSet or not.
    * 
    * On average, this is an O(1) operation; the (unlikely) worst-case
    * is O(n).
    */
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
    if (i < buckets.length) VPtr[this.type](i) else Ptr.Null[this.type]
  }

  final def ptrNext(ptr: VPtr[this.type]): Ptr[this.type] = {
    var i = ptr.raw.toInt + 1
    while (i < buckets.length && buckets(i) != 3) i += 1
    if (i < buckets.length) VPtr[this.type](i) else Ptr.Null[this.type]
  }

  final def ptrKey[@specialized L](ptr: VPtr[this.type]): L =
    keys.asInstanceOf[Array[L]](ptr.raw.toInt)

  final def ptrElement1[@specialized E1](ptr: VPtr[this.type]): E1 =
    keys.asInstanceOf[Array[E1]](ptr.raw.toInt)

}

object HashSet {

  /** Unused bucket. */
  @inline final def UNUSED: Byte = 0

  /** Once used, has been deleted but not yet overwritten. */
  @inline final def DELETED: Byte = 2

  /** Used. */
  @inline final def USED: Byte = 3

}

trait HashSetFactory extends SetFactory {

  type Extra[K] = Dummy[K]

  type S[K] <: generic.HashSet[K]

}
