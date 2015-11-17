package metal
package impl

import scala.reflect.ClassTag

import spire.algebra.Order

import syntax._

final class SortedSetImpl[K](var items: Array[K], var size: Long)(implicit val K: Methods[K], val order: Order[K]) extends ISortedSet[K] with MSortedSet[K] {

  type IType = SortedSetImpl[K]
  type MType = SortedSetImpl[K]

  @inline final def isEmpty = size == 0
  @inline final def nonEmpty = size > 0

  def keyArray(ptr: MyVPtr): Array[K] = items
  def keyIndex(ptr: MyVPtr): Int = ptr.raw.toInt

  def result(): ISortedSet[K] with IType = this

  def absorb(newItems: Array[K], newSize: Long): Unit = {
    items = newItems

    size = newSize
  }

  def mutableCopy: MSortedSet[K] with MType = {
    val res = new SortedSetImpl[K](null, 0)
    res.absorb(items.clone, size)
    res
  }

  protected def findWhere[@specialized L](item: L): Int = {
    val itemsL = items.asInstanceOf[Array[L]]
    val orderL = order.asInstanceOf[Order[L]]
    var lb = 0
    var ub = size.toInt
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
    if (lb == size) return ~(size.toInt)
    val c = orderL.compare(itemsL(lb), item)
    if (c == 0) return lb
    if (c > 0) return ~lb
    sys.error("Should not happen")
  }

  final def ptrRemoveAndAdvance(ptr: MyVPtr): MyPtr = {
    val pos = ptr.raw.toInt
    java.lang.System.arraycopy(items, pos + 1, items, pos, size.toInt - pos - 1)
    size -= 1
    items(size.toInt) = null.asInstanceOf[K]
    if (pos >= size) Ptr.`null`(this) else ptr
  }

  final def ptrRemove(ptr: MyVPtr): Unit = ptrRemoveAndAdvance(ptr)

  @inline final def ptrFind[@specialized L](key: L): MyPtr = {
    val ind = findWhere[L](key)
    if (ind >= 0) Ptr(this, ind) else Ptr.`null`(this)
  }

  @inline final def ptrAddKey[@specialized L](key: L): MyVPtr = {
    val itemsL = items.asInstanceOf[Array[L]]
    val pos = findWhere[L](key)
    if (pos < 0) {
      val ipos = ~pos
      val newItemsL = if (size < itemsL.length) itemsL else {
        val arr = (K.newArray(itemsL.length.max(1) * 2)).asInstanceOf[Array[L]]
        java.lang.System.arraycopy(itemsL, 0, arr, 0, ipos)
        arr
      }
      java.lang.System.arraycopy(itemsL, ipos, newItemsL, ipos + 1, size.toInt - ipos)
      newItemsL(ipos) = key
      items = newItemsL.asInstanceOf[Array[K]]
      size += 1
      VPtr(this, ipos)
    } else VPtr(this, pos)
  }

  @inline final def ptr: MyPtr = if (size == 0) Ptr.`null`(this) else Ptr(this, 0)
  @inline final def ptrNext(ptr: MyVPtr): MyPtr = if (ptr.raw == size - 1) Ptr.`null`(this) else Ptr(this, ptr.raw + 1)
  @inline final def ptrKey[@specialized L](ptr: MyVPtr): L = items.asInstanceOf[Array[L]](ptr.raw.toInt)
  @inline final def ptrElement[@specialized E](ptr: MyVPtr): E = items.asInstanceOf[Array[E]](ptr.raw.toInt)
}

object SortedSetImpl {

  private[metal] def ofAllocatedSize[K:Order](n: Int)(implicit K: Methods[K]) = {
    import K.classTag
    new SortedSetImpl[K](
      items = K.newArray(n),
      size = 0)
  }

}
