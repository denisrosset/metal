import scala.language.experimental.macros

import scala.annotation.tailrec
import scala.{specialized => spec}

import machinist.DefaultOps

trait GenHasPtr[P <: Int] {
  def next(ptr: P): P
  def hasAt(ptr: P): Boolean
}

trait HasPtr[A, P <: Int] extends GenHasPtr[P] {
  def at(ptr: P): A
}

class HasPtrOps[P](val lhs: P) extends AnyVal  {
  def next(implicit ev: GenHasPtr[P]): P = macro DefaultOps.unopWithEv[GenHasPtr[P], P]
  def hasAt(implicit ev: GenHasPtr[P]): Boolean = macro DefaultOps.unopWithEv[GenHasPtr[P], P]
  def at[A](implicit ev: HasPtr[A, P]): A = macro DefaultOps.unopWithEv[HasPtr[A, P], A]
}

object Support {
  implicit def toPtrOps[P](lhs: P): HasPtrOps[P] = new HasPtrOps(lhs)

  type Tagged[U] = { type Tag = U }
  type @@[T, U] = T with Tagged[U]
}

case class SetInt(set: Set[Int]) extends HasPtr[Int, Int] {
  import Support._

  trait Tag
  type Ptr = Int @@ Tag

  def pointer: Ptr = if (set.isEmpty) -1.asInstanceOf[Ptr] else set.min.asInstanceOf[Ptr]

  def next(ptr: Int) = {
    val rest = set.filter(_ > ptr)
    if (rest.isEmpty) -1 else rest.min
  }
  def hasAt(ptr: Int) = ptr != -1
  def at(ptr: Int) = if (ptr.asInstanceOf[Int] == -1) Iterator.empty.next else ptr.asInstanceOf[Int]

  implicit def HasPtr: HasPtr[Int, Ptr] = this.asInstanceOf[HasPtr[Int, Ptr]]
}


object Test extends App {
  import Support._

  def test1: Unit = {
    val set1 = SetInt(Set(0,2,3,5,6,10))
    import set1.{HasPtr => HasPtr1}
    val set2 = SetInt(Set(1,2,3,4))
    import set2.{HasPtr => HasPtr2}

    var ptr1 = set1.pointer
    while (ptr1.hasAt) {
      println(ptr1.at)
      ptr1 = ptr1.next
    }
    var ptr2 = set2.pointer
    while (ptr2.hasAt) {
      ptr2 + 1
      println(ptr2.at)
      ptr2 = ptr2.next
    }
  }
  test1
}
