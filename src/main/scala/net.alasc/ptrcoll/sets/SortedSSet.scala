package net.alasc.ptrcoll
package sets

import scala.reflect.ClassTag

import spire.algebra.Order

import syntax.all._

trait SortedSSet[@specialized(Int) A] extends SSet[A] { self =>
  implicit def order: Order[A]
}

trait SortedSSetImpl[@specialized(Int) A] extends SortedSSet[A] with PointableAtImpl[A] { self =>
  var items: Array[A]
  var size: Int

  protected def findWhere(item: A): Int = {
    var lb = 0
    var ub = size
    while (lb < ub) {
      val m = (lb + ub) >>> 1
      val c = order.compare(items(m), item)
      if (c == 0) return m
      if (c < 0)
        lb = m + 1
      else
        ub = m
    }
    // now lb == ub
    if (lb == size) return ~size
    val c = order.compare(items(lb), item)
    if (c == 0) return lb
    if (c > 0) return ~lb
    sys.error("Should not happen")
  }

  def remove(item: A): Boolean = {
    val pos = findWhere(item)
    if (pos >= 0) {
      java.lang.System.arraycopy(items, pos + 1, items, pos, size - pos - 1)
      size -= 1
      true
    } else false
  }

  def apply(item: A): Boolean = findWhere(item) >= 0

  def add(item: A): Boolean = {
    val pos = findWhere(item)
    if (pos < 0) {
      val ipos = ~pos
      val newItems = if (size < items.length) items else {
        val arr = new Array[A](items.length * 2)
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

  def pointer: Ptr = if (size == 0) Ptr(-1) else Ptr(0)

    // hidden by SortedSSet cast
  def next(ptr: RawPtr) = if (ptr == size - 1) Ptr(-1) else Ptr(ptr + 1)
  def at(ptr: RawPtr): A = items(ptr.toInt)
  def hasAt(ptr: RawPtr) = ptr >= 0 && ptr < size
}

object SortedSSet {
  def empty[A:Order:ClassTag]: SortedSSet[A] = new SortedSSetImpl[A] {
    def ct = implicitly[ClassTag[A]]
    def order = Order[A]
    var items = new Array[A](8)
    var size = 0
  }
  def apply[A:Order:ClassTag](items: A*): SortedSSet[A] = {
    val s = empty[A]
    items.foreach { a => s += a }
    s
  }
}
