package metal
package immutable

import metal.syntax._

/** Wraps a metal collection as a Scala collection.
  * 
  * Requirement: the size can fit in an `Int`.
  */
trait Wrapped[E, +W <: metal.immutable.Collection, +S <: scala.collection.immutable.Iterable[E]]
    extends scala.collection.immutable.Iterable[E] {

  type W <: metal.immutable.Collection
  type S <: scala.collection.immutable.Iterable[E]
  val w: W

  def element(ptr: VPtr[w.type]): E

  def iterator: Iterator[E] = new Iterator[E] {
    private[this] var ptr: Ptr[w.type] = w.ptr
    def hasNext = ptr.nonNull
    def next(): E = ptr match {
      case IsVPtr(vp) =>
        ptr = w.ptrNext(vp)
        element(vp)
      case _ => Iterator.empty.next
    }
  }

  override def size: Int = w.longSize.toInt

}

trait Wrapped1[E1, +W <: metal.immutable.Collection with NElements1[E1], +S <: scala.collection.immutable.Iterable[E1]](w0: W)
    extends Wrapped[E1, W, S](w0) {

  def element(ptr: VPtr[w.type]): E1 = w.ptrElement1(ptr)

  override def foreach[U](f: E1 => U): Unit =
    w.foreach( e1 => f(e1) )

  override def forall(f: E1 => Boolean): Boolean =
    w.forall( e1 => f(e1) )

  override def exists(f: E1 => Boolean): Boolean =
    w.exists( e1 => f(e1) )

  override def count(f: E1 => Boolean): Int =
    w.count( e1 => f(e1) ).toInt

  override def foldLeft[B](z: B)(op: (B, E1) => B): B =
    w.foldLeft(z)(op)

  override def /:[B](z: B)(op: (B, E1) => B): B =
    w.foldLeft(z)(op)

}

abstract class WrappedSet[K, +W <: metal.immutable.Set[K], +S <: scala.collection.immutable.Set[K]](w0: W)
    extends Wrapped1[K, W, S](w0) with scala.collection.immutable.Set[K] {

  def myBuilder: scala.collection.mutable.Builder[K, S]

  def -(elem: K): S = {
    val b = myBuilder
    w.foreach { k => if (k != elem) b += k }
    b.result()
  }

  def +(elem: K): S = {
    val b = myBuilder
    w.foreach { k => b += k }
    b += elem
    b.result()
  }

  def contains(elem: K): Boolean = w.contains(elem)

}

class WrappedSortedSetRange[K, W <: metal.immutable.SortedSet[K] with Singleton](w: W, fromPtr: Ptr[W], untilPtr: Ptr[W]) extends scala.collection.immutable.SortedSet[K] {

  implicit def ordering = spire.compat.ordering(w.order)

  def -(elem: K) = (scala.collection.immutable.SortedSet.empty[K] ++ iterator) - elem

  def +(elem: K) = (scala.collection.immutable.SortedSet.empty[K] ++ iterator) + elem

  def contains(elem: K) = {
    val res = (w: W).ptrFind(elem)
    res >= fromPtr && res < untilPtr
  }

  def iterator = iteratorFrom(fromPtr)

  def iteratorFrom(otherFromPtr: Ptr[W]): Iterator[K] = {
    val firstPtr = fromPtr.max(otherFromPtr)
    if (firstPtr >= untilPtr) Iterator.empty else new Iterator[K] {
      private[this] var ptr: Ptr[W] = firstPtr
      def hasNext = ptr != untilPtr
      def next(): K = ptr match {
        case IsVPtr(vp) =>
          ptr = (w:W).ptrNext(vp)
          (w:W).ptrKey(vp)
        case _ => Iterator.empty.next
      }
    }
  }

  def keysIteratorFrom(start: K): Iterator[K] = iteratorFrom((w:W).findOrNextAfter(start))

  def rangeImpl(from: Option[K], until: Option[K]): scala.collection.immutable.SortedSet[K] = {
    val newFromPtr = from.fold((w:W).ptr)(item => (w:W).findOrNextAfter(item)).max(fromPtr)
    val newUntilPtr = until.fold((w:W).ptr)(item => (w:W).findOrNextAfter(item)).min(untilPtr)
    new WrappedSortedSetRange[K, W](w, fromPtr, untilPtr)
  }

}

abstract class WrappedSortedSet[K, W <: metal.immutable.SortedSet[K], +S <: scala.collection.immutable.SortedSet[K]](w0: W)
    extends WrappedSet[K, W, S](w0) with scala.collection.immutable.SortedSet[K] {

  implicit def ordering: Ordering[K] = spire.compat.ordering(w.order)

  def keysIteratorFrom(start: K): Iterator[K] = new Iterator[K] {
    private[this] var ptr: Ptr[w.type] = w.findOrNextAfter(start)
    def hasNext = ptr.nonNull
    def next(): K = ptr match {
      case IsVPtr(vp) =>
        ptr = w.ptrNext(vp)
        w.ptrKey(vp)
      case _ => Iterator.empty.next
    }
  }

  def rangeImpl(from: Option[K], until: Option[K]): scala.collection.immutable.SortedSet[K] = {
    val fromPtr = from.fold(w.ptr)(item => w.findOrNextAfter(item))
    val untilPtr = until.fold(w.ptr)(item => w.findOrNextAfter(item))
    new WrappedSortedSetRange[K, w.type](w, fromPtr, untilPtr)
  }

}

abstract class WrappedBitSet(w0: metal.immutable.BitSet) extends WrappedSortedSet[Int, metal.immutable.BitSet, scala.collection.immutable.BitSet] with scala.collection.immutable.BitSet {

}
