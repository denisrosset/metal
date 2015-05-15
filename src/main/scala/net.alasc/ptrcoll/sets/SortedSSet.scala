package net.alasc.ptrcoll
package sets

import scala.{specialized => sp}
import scala.reflect.ClassTag

import spire.algebra.Order

import syntax.all._

trait SortedSSet[@specialized(Int) K] extends MutSSet[K] { self =>
  implicit def orderK: Order[K]
  def copy: SortedSSet[K]
}

class SortedSSetImpl[@specialized(Int) K](var items: Array[K], var size: Int)(implicit val ctK: ClassTag[K], val orderK: Order[K]) extends SortedSSet[K] with HasPtrAt[K, RawPtr] { self =>

  @inline final def isEmpty = size == 0
  @inline final def nonEmpty = size > 0
  def copy: SortedSSet[K] = new SortedSSetImpl[K](items.clone, size)(ctK, orderK)

  protected def findWhere(item: K): Int = {
    var lb = 0
    var ub = size
    while (lb < ub) {
      val m = (lb + ub) >>> 1
      val c = orderK.compare(items(m), item)
      if (c == 0) return m
      if (c < 0)
        lb = m + 1
      else
        ub = m
    }
    // now lb == ub
    if (lb == size) return ~size
    val c = orderK.compare(items(lb), item)
    if (c == 0) return lb
    if (c > 0) return ~lb
    sys.error("Should not happen")
  }

  final def contains(item: K) = findWhere(item) >= 0

  final def -=(item: K): this.type = { remove(item); this }
  final def +=(item: K): this.type = { add(item); this }

  final def remove(item: K): Boolean = {
    val ptr = findPointerAt(item)
    if (hasAt(ptr)) {
      removeAt(ptr.asInstanceOf[ValidPtr])
      true
    } else false
  }

  final def removeAndAdvance(ptr: ValidPtr): Ptr = {
    val pos = ptr.toInt
    java.lang.System.arraycopy(items, pos + 1, items, pos, size - pos - 1)
    size -= 1
    if (ptr >= size) nullPtr else ptr
  }

  final def removeAt(ptr: ValidPtr): Unit = removeAndAdvance(ptr)

  def findPointerAt(item: K): Ptr = {
    val ind = findWhere(item)
    if (ind >= 0) Ptr(ind) else nullPtr
  }

  def add(item: K): Boolean = {
    val pos = findWhere(item)
    if (pos < 0) {
      val ipos = ~pos
      val newItems = if (size < items.length) items else {
        val arr = new Array[K](items.length * 2)
        java.lang.System.arraycopy(items, 0, arr, 0, ipos)
        arr
      }
      java.lang.System.arraycopy(items, ipos, newItems, ipos + 1, size - ipos)
      items = newItems
      items(ipos) = item
      size += 1
      false
    } else true
  }

  @inline final def nullPtr: Ptr = Ptr(-1L)
  @inline final def pointer: Ptr = if (size == 0) nullPtr else Ptr(0)
  @inline final def Ptr(rawPtr: RawPtr) = rawPtr.asInstanceOf[Ptr]
    // hidden by SortedSSet cast
  @inline final def nextPtr(ptr: RawPtr) = if (ptr == size - 1) nullPtr else Ptr(ptr + 1)
  @inline final def at(ptr: RawPtr): K = items(ptr.toInt)
  @inline final def hasAt(ptr: RawPtr) = ptr >= 0 && ptr < size

  @inline final implicit def PtrTC: HasPtrAt[K, Ptr] = self.asInstanceOf[HasPtrAt[K, Ptr]]
}

object SortedSSet extends MutSSetFactory[Any, Order] {
  def empty[@sp(Int) K](implicit c: ClassTag[K], ord: Order[K], e: LBEv[K]): SortedSSet[K] = new SortedSSetImpl[K](
    items = new Array[K](8),
    size = 0)
  def apply[@sp(Int) K](items: K*)(implicit ct: ClassTag[K], ord: Order[K], e: LBEv[K]): SortedSSet[K] = {
    val s = empty[K](ct, ord, e)
    items.foreach { a => s += a }
    s
  }
  def ofSize[@sp(Int) K](n: Int)(implicit c: ClassTag[K], ord: Order[K], e: LBEv[K]): SortedSSet[K] = new SortedSSetImpl[K](
    items = new Array[K](n),
    size = 0)
}
