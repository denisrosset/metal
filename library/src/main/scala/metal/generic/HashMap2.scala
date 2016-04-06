package metal
package generic

import scala.annotation.tailrec

import util.Dummy

abstract class HashMap2[K, V1, V2] extends generic.Map2[K, V1, V2] {

  import HashMap2.{UNUSED, USED}

  type Immutable = immutable.HashMap2[K, V1, V2]
  type Mutable = mutable.HashMap2[K, V1, V2]
  type Scala = metal.immutable.WrappedHashMap2[K, V1, V2]

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
  def value1(i: Int): V1 = value1(i)
  def value2(i: Int): V2 = value2(i)

  /** Status of the slots in the hash table. */
  private[metal] def buckets: Array[Byte]

  /** Slots for keys. */
  private[metal] def keys: Array[K]

  /** Slots for values of type V1. */
  private[metal] def values1: Array[V1]

  /** Slots for values of type V2. */
  private[metal] def values2: Array[V2]

  def mutableCopy =
    new mutable.HashMap2[K, V1, V2](
      keys.clone, buckets.clone,
      values1.clone, values2.clone,
      size, used, mask, limit)

  @inline final def isEmpty = size == 0
  @inline final def nonEmpty = size > 0

  def keyArray(ptr: VPtr[this.type]): Array[K] = keys
  def keyIndex(ptr: VPtr[this.type]): Int = ptr.raw.toInt

  def value1Array(ptr: VPtr[this.type]): Array[V1] = values1
  def value1Index(ptr: VPtr[this.type]): Int = ptr.raw.toInt

  def value2Array(ptr: VPtr[this.type]): Array[V2] = values2
  def value2Index(ptr: VPtr[this.type]): Int = ptr.raw.toInt

  final def ptrFind[@specialized L](key: L): Ptr[this.type] = {
    val keysL = keys.asInstanceOf[Array[L]]
    @inline @tailrec def loop(i: Int, perturbation: Int): Ptr[this.type] = {
      val j = i & mask
      val status = buckets(j)
      if (status == UNUSED) Ptr.Null(this)
      else if (status == USED && keysL(j) == key) VPtr(this, j)
      else loop((i << 2) + i + perturbation + 1, perturbation >> 5)
    }
    val i = K.asInstanceOf[MetalTag[L]].hash(key) & 0x7fffffff
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

  final def ptrValue1[@specialized W1](ptr: VPtr[this.type]): W1 = values1.asInstanceOf[Array[W1]](ptr.raw.toInt)

  final def ptrValue2[@specialized W2](ptr: VPtr[this.type]): W2 = values2.asInstanceOf[Array[W2]](ptr.raw.toInt)

  final def ptrElement1[@specialized E1](ptr: VPtr[this.type]): E1 = keys.asInstanceOf[Array[E1]](ptr.raw.toInt)

  final def ptrElement2[@specialized E2](ptr: VPtr[this.type]): E2 = values1.asInstanceOf[Array[E2]](ptr.raw.toInt)

  final def ptrElement3[@specialized E3](ptr: VPtr[this.type]): E3 = values2.asInstanceOf[Array[E3]](ptr.raw.toInt)

}

object HashMap2 {

  /** Unused bucket. */
  @inline final def UNUSED: Byte = 0

  /** Once used, has been deleted but not yet overwritten. */
  @inline final def DELETED: Byte = 2

  /** Used. */
  @inline final def USED: Byte = 3

}

trait HashMap2Factory extends Map2Factory {

  type KExtra[K] = Dummy[K]
  type V1Extra[V1] = Dummy[V1]
  type V2Extra[V2] = Dummy[V2]

  type M[K, V1, V2] <: generic.HashMap2[K, V1, V2]

}
