package metal
package immutable

import scala.annotation.tailrec
import scala.reflect.ClassTag

import spire.algebra.Order

import metal.syntax._

final class WrappedArraySortedSet[K](val w: metal.immutable.SortedSet[K])
    extends scala.collection.immutable.SortedSet[K]
    with scala.collection.SortedSetLike[K, WrappedArraySortedSet[K]]
    with WrappedSortedSet[K] {

  import w.{ctK, order}

  override def empty: WrappedArraySortedSet[K] = new WrappedArraySortedSet(metal.immutable.ArraySortedSet.empty[K])

  def -(elem: K): WrappedArraySortedSet[K] =
    if (!w.contains(elem)) this else {
      val b = w.mutableCopy
      b -= elem
      new WrappedArraySortedSet(b.result())
    }

  def +(elem: K): WrappedArraySortedSet[K] =
    if (w.contains(elem)) this else {
      val b = w.mutableCopy
      b += elem
      new WrappedArraySortedSet(b.result())
    }

  def iterator: Iterator[K] = new Iterator[K] {
    private[this] var ptr: Ptr[w.type] = w.ptr
    def hasNext = ptr.nonNull
    def next(): K = ptr match {
      case IsVPtr(vp) =>
        ptr = w.ptrNext(vp)
        w.ptrKey(vp)
      case _ => Iterator.empty.next
    }
  }

  def contains(elem: K) = w.contains(elem)

  def ordering: Ordering[K] = spire.compat.ordering(w.order)

  def keysIteratorFrom(start: K) = new Iterator[K] {
    private[this] var ptr: Ptr[w.type] = w.findOrNextAfter(start)
    def hasNext = ptr.nonNull
    def next(): K = ptr match {
      case IsVPtr(vp) =>
        ptr = w.ptrNext(vp)
        w.ptrKey(vp)
      case _ => Iterator.empty.next
    }
  }

  def rangeImpl(from: Option[K], until: Option[K]): WrappedArraySortedSet[K] = {
    val fromPtr = from.fold(w.ptr)(item => w.findOrNextAfter(item))
    val untilPtr = until.fold(w.ptr)(item => w.findOrNextAfter(item))
    if (fromPtr < untilPtr) {
      val b = newBuilder
      @tailrec def rec(current: Ptr[w.type]): Unit = current match {
        case IsVPtr(vp) =>
          if (current < untilPtr) {
            b += w.ptrKey(vp)
            rec(w.ptrNext(vp))
          }
        case _ =>
      }
      rec(fromPtr)
      b.result()
    } else empty
  }

  override def newBuilder: scala.collection.mutable.Builder[K, WrappedArraySortedSet[K]] = new scala.collection.mutable.Builder[K, WrappedArraySortedSet[K]] {
    private[this] var current: metal.mutable.ArraySortedSet[K] = metal.mutable.ArraySortedSet.empty[K]
    def clear() = { current = metal.mutable.ArraySortedSet.empty[K] }
    def +=(elem: K) = { val c = current; c += elem; this }
    def result() = new WrappedArraySortedSet[K](current.result())
  }

}

final class ArraySortedSet[K](
  private[metal] val items: Array[K],
  val longSize: Long)(implicit
    val ctK: ClassTag[K],
    val K: MetalTag[K],
    val order: Order[K]
) extends generic.ArraySortedSet[K] with immutable.SortedSet[K] {

  def toScala = new WrappedArraySortedSet[K](this)

}

object ArraySortedSet extends immutable.SetFactory {

  type Extra[K] = Order[K]
  type S[K] = immutable.ArraySortedSet[K]
  type MS[K] = mutable.ArraySortedSet[K]

  def mutableFactory = mutable.ArraySortedSet

}
