package metal
package impl

import scala.reflect.ClassTag

import spire.algebra.Order

final class SortedSetImpl[K](var items: Array[K], var longSize: Long)(implicit val K: Methods[K], val order: Order[K]) extends ISortedSet[K] with MSortedSet[K] {

  type IType = SortedSetImpl[K]
  type MType = SortedSetImpl[K]

  @inline final def isEmpty = longSize == 0
  @inline final def nonEmpty = longSize > 0

  def keyArray(ptr: VPtr[this.type]): Array[K] = items
  def keyIndex(ptr: VPtr[this.type]): Int = ptr.raw.toInt

  def result(): ISortedSet[K] with IType = this

  def absorb(newItems: Array[K], newSize: Long): Unit = {
    items = newItems
    longSize = newSize
  }

  def mutableCopy: MSortedSet[K] with MType = {
    val res = new SortedSetImpl[K](null, 0)
    res.absorb(items.clone, longSize)
    res
  }

  def clear(): Unit = {
    absorb(K.newArray(0), 0)
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

  final def ptrRemoveAndAdvance(ptr: VPtr[this.type]): Ptr[this.type] = {
    val pos = ptr.raw.toInt
    java.lang.System.arraycopy(items, pos + 1, items, pos, longSize.toInt - pos - 1)
    longSize -= 1
    items(longSize.toInt) = null.asInstanceOf[K]
    if (pos >= longSize) Ptr.Null(this) else ptr
  }

  final def ptrRemove(ptr: VPtr[this.type]): Unit = ptrRemoveAndAdvance(ptr)

  @inline final def ptrFind[@specialized L](key: L): Ptr[this.type] = {
    val ind = findWhere[L](key)
    if (ind >= 0) Ptr(this, ind) else Ptr.Null(this)
  }

  @inline final def ptrAddKey[@specialized L](key: L): VPtr[this.type] = {
    val itemsL = items.asInstanceOf[Array[L]]
    val pos = findWhere[L](key)
    if (pos < 0) {
      val ipos = ~pos
      val newItemsL = if (longSize < itemsL.length) itemsL else {
        val arr = (K.newArray(itemsL.length.max(1) * 2)).asInstanceOf[Array[L]]
        java.lang.System.arraycopy(itemsL, 0, arr, 0, ipos)
        arr
      }
      java.lang.System.arraycopy(itemsL, ipos, newItemsL, ipos + 1, longSize.toInt - ipos)
      newItemsL(ipos) = key
      items = newItemsL.asInstanceOf[Array[K]]
      longSize += 1
      VPtr(this, ipos)
    } else VPtr(this, pos)
  }

  @inline final def ptr: Ptr[this.type] = if (longSize == 0) Ptr.Null(this) else Ptr(this, 0)
  @inline final def ptrNext(ptr: VPtr[this.type]): Ptr[this.type] = if (ptr.raw == longSize - 1) Ptr.Null(this) else Ptr(this, ptr.raw + 1)
  @inline final def ptrKey[@specialized L](ptr: VPtr[this.type]): L = items.asInstanceOf[Array[L]](ptr.raw.toInt)
  @inline final def ptrElement1[@specialized E1](ptr: VPtr[this.type]): E1 = items.asInstanceOf[Array[E1]](ptr.raw.toInt)
}

object SortedSetImpl {

  private[metal] def ofAllocatedSize[K:Order](n: Int)(implicit K: Methods[K]) = {
    import K.classTag
    new SortedSetImpl[K](
      items = K.newArray(n),
      longSize = 0)
  }

}
