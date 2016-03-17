package metal

import spire.algebra.Order

/** Sorted set backed by an array of sorted keys. */
abstract class ArraySortedSet[K] extends SortedSet[K] {

  private[metal] def items: Array[K]

  type Immutable = metal.immutable.ArraySortedSet[K]
  type Mutable = metal.mutable.ArraySortedSet[K]

  @inline final def isEmpty = longSize == 0
  @inline final def nonEmpty = longSize > 0

  private[metal] def keyArray(ptr: VPtr[this.type]): Array[K] = items
  private[metal] def keyIndex(ptr: VPtr[this.type]): Int = ptr.raw.toInt

  def mutableCopy: Mutable ={
    val res = new mutable.ArraySortedSet[K](null, 0)
    res.absorb(items.clone, longSize)
    res
  }

  protected def findWhere[@specialized L](item: L): Int = {
    val itemsL = items.asInstanceOf[Array[L]]
    val orderL = order.asInstanceOf[Order[L]]
    var lb = 0
    var ub = longSize.toInt
    while (lb < ub) {
      val m = (lb + ub) >>> 1
      val c = orderL.compare(itemsL(m), item)
      if (c == 0) return m
      if (c < 0)
        lb = m + 1
      else
        ub = m
    }
    // now lb == ub
    if (lb == longSize) return ~(longSize.toInt)
    val c = orderL.compare(itemsL(lb), item)
    if (c == 0) return lb
    if (c > 0) return ~lb
    sys.error("Should not happen")
  }

  @inline final def ptrFind[@specialized L](key: L): Ptr[this.type] = {
    val ind = findWhere[L](key)
    if (ind >= 0) Ptr(this, ind) else Ptr.Null(this)
  }

  @inline final def ptr: Ptr[this.type] = if (longSize == 0) Ptr.Null(this) else Ptr(this, 0)
  @inline final def ptrNext(ptr: VPtr[this.type]): Ptr[this.type] = if (ptr.raw == longSize - 1) Ptr.Null(this) else Ptr(this, ptr.raw + 1)
  @inline final def ptrKey[@specialized L](ptr: VPtr[this.type]): L = items.asInstanceOf[Array[L]](ptr.raw.toInt)
  @inline final def ptrElement1[@specialized E1](ptr: VPtr[this.type]): E1 = items.asInstanceOf[Array[E1]](ptr.raw.toInt)

}
