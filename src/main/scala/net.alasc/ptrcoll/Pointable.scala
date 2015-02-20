package net.alasc.ptrcoll

import scala.annotation.tailrec
import scala.{specialized => sp}

trait WithPointer {
  /** Tag of this instance, used to select the correct typeclass for
    * the pointer syntax. */
  trait Tag
  /** Tagged pointer type for this collection instance. */
  type Ptr = TaggedPtr[Tag]
  def nullPtr: Ptr
  final def Ptr(r: RawPtr): Ptr = r.asInstanceOf[Ptr]
  /** Pointer typeclass. */
  implicit def PtrTC: HasPtr[Ptr]
}

trait WithPointerAt[@sp(Int) A] extends WithPointer {
  implicit def PtrTC: HasPtrAt[A, Ptr]
}

/** Iterable-like trait for fast unboxed iteration over collections. */
trait Pointable extends WithPointer {
  /** Returns a pointer for this collection instance. */
  def pointer: Ptr
  /** Returns true if the collection is empty, false otherwise. */
  def isEmpty: Boolean = !PtrTC.hasAt(pointer)
  /** Returns true if the collection is non-empty, false otherwise. */
  def nonEmpty: Boolean = PtrTC.hasAt(pointer)
}

trait PointableAt[@sp(Int) A] extends Pointable with WithPointerAt[A]

object PointableConversions {
  implicit def pointableToIterable[A](p: PointableAt[A]): Iterable[A] = new Iterable[A] {

    import p.{Ptr, PtrTC}
    import syntax.hasPtr._

    def iterator: Iterator[A] = if (p.isEmpty) Iterator.empty else new Iterator[A] {
      private[this] var ptr = p.pointer
      def hasNext = ptr.next.hasAt
      def next: A = {
        val res = ptr.at
        ptr = ptr.next
        res
      }
    }
    override def size: Int = {
      @tailrec def loop(ptr: Ptr, acc: Int): Int =
        if (ptr.hasAt) loop(ptr.next, acc + 1) else acc
      loop(p.pointer, 0)
    }
    override def foreach[U](f: A => U): Unit = {
      var ptr = p.pointer
      while(ptr.hasAt) {
        f(ptr.at)
        ptr = ptr.next
      }
    }
    override def forall(pred: A => Boolean): Boolean = {
      @tailrec def loop(ptr: Ptr): Boolean =
        if (ptr.hasAt) {
          if (pred(ptr.at)) loop(ptr.next) else false
        } else true
      loop(p.pointer)
    }
    override def exists(pred: A => Boolean): Boolean = {
      @tailrec def loop(ptr: Ptr): Boolean =
        if (ptr.hasAt) {
          if (pred(ptr.at)) true else loop(ptr.next)
        } else false
      loop(p.pointer)
    }
  }
}

trait PointableImpl extends Pointable with HasPtr[RawPtr] { self =>
  def next(ptr: RawPtr): RawPtr
  def hasAt(ptr: RawPtr): Boolean
  /** Pointable is its own typeclass (magic!). */
  def PtrTC: HasPtr[Ptr] = self.asInstanceOf[HasPtr[Ptr]]
}

trait PointableAtImpl[@specialized(Int) A] extends PointableAt[A] with HasPtrAt[A, RawPtr] { self =>
  def at(ptr: Long): A
  implicit def PtrTC: HasPtrAt[A, Ptr] = self.asInstanceOf[HasPtrAt[A, Ptr]]
}
