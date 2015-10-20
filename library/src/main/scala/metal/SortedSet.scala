package metal

import scala.reflect.ClassTag

import spire.algebra.Order

import syntax._

trait ISortedSet[K] extends ISet[K] { self =>

  def copy: ISortedSet[K]
}

trait SortedSet[K] extends Set[K] with ISortedSet[K] { self =>

  implicit def orderK: Order[K]

  def copy: SortedSet[K]

}

final class SortedSetImpl[K](allocatedSize: Int)(implicit val ctK: ClassTag[K], val orderK: Order[K]) extends SortedSet[K] {

  var items: Array[K] = new Array[K](allocatedSize)
  var size: Int = 0
  @inline final def isEmpty = size == 0
  @inline final def nonEmpty = size > 0

  def absorb(newItems: Array[K], newSize: Int): Unit = {
    items = newItems
    size = newSize
  }

  def copy: SortedSet[K] = {
    val res = new SortedSetImpl[K](0)(ctK, orderK)
    res.absorb(items.clone, size)
    res
  }

  protected def findWhere[@specialized L](item: L): Int = {
    val itemsL = items.asInstanceOf[Array[L]]
    val orderL = orderK.asInstanceOf[Order[L]]
    var lb = 0
    var ub = size
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
    if (lb == size) return ~size
    val c = orderL.compare(itemsL(lb), item)
    if (c == 0) return lb
    if (c > 0) return ~lb
    sys.error("Should not happen")
  }

  final def ptrRemoveAndAdvance(ptr: VPtr[Tag]): Ptr[Tag] = {
    val pos = ptr.v.toInt
    java.lang.System.arraycopy(items, pos + 1, items, pos, size - pos - 1)
    size -= 1
    if (pos >= size) Ptr.Null[Tag] else ptr
  }

  final def ptrRemove(ptr: VPtr[Tag]): Unit = ptrRemoveAndAdvance(ptr)

  @inline final def ptrFind[@specialized L](key: L): Ptr[Tag] = {
    val ind = findWhere[L](key)
    if (ind >= 0) VPtr[Tag](ind) else Ptr.Null[Tag]
  }

  @inline final def ptrAddKey[@specialized L](key: L): VPtr[Tag] = {
    val itemsL = items.asInstanceOf[Array[L]]
    val pos = findWhere[L](key)
    if (pos < 0) {
      val ipos = ~pos
      val newItemsL = if (size < itemsL.length) itemsL else {
        val arr = (new Array[K](itemsL.length * 2)).asInstanceOf[Array[L]]
        java.lang.System.arraycopy(itemsL, 0, arr, 0, ipos)
        arr
      }
      java.lang.System.arraycopy(itemsL, ipos, newItemsL, ipos + 1, size - ipos)
      newItemsL(ipos) = key
      items = newItemsL.asInstanceOf[Array[K]]
      size += 1
      VPtr[Tag](ipos)
    } else VPtr[Tag](pos)
  }

  @inline final def ptr: Ptr[Tag] = if (size == 0) Ptr.Null[Tag] else VPtr[Tag](0)
  @inline final def ptrNext(ptr: VPtr[Tag]): Ptr[Tag] = if (ptr.v == size - 1) Ptr.Null[Tag] else VPtr[Tag](ptr.v + 1)
  @inline final def ptrKey[@specialized L](ptr: VPtr[Tag]): L = items.asInstanceOf[Array[L]](ptr.v.toInt)
}

object SortedSet extends SetFactory[Any, Order] {
  def empty[K](implicit c: ClassTag[K], ord: Order[K], e: LBEv[K]): SortedSet[K] = new SortedSetImpl[K](8)
  def apply[K](items: K*)(implicit ct: ClassTag[K], ord: Order[K], e: LBEv[K]): SortedSet[K] = {
    val s = empty[K](ct, ord, e)
    items.foreach { a => s += a }
    s
  }
  def ofSize[K](n: Int)(implicit c: ClassTag[K], ord: Order[K], e: LBEv[K]): SortedSet[K] = new SortedSetImpl[K](n)
}
