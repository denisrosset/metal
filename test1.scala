import scala.language.experimental.macros

import scala.annotation.tailrec

import machinist.DefaultOps

trait HasIterGen[I] {
  def next(i: I): I
  def hasNext(i: I): Boolean
}

trait HasIter[A, I] extends HasIterGen[I] {
  def there(i: I): A
}

class HasIterOps[I](val lhs: I) extends AnyVal  {
  def next(implicit ev: HasIterGen[I]): I = macro DefaultOps.unopWithEv[HasIterGen[I], I]
  def hasNext(implicit ev: HasIterGen[I]): Boolean = macro DefaultOps.unopWithEv[HasIterGen[I], I]
  def there[A](implicit ev: HasIter[A, I]): A = macro DefaultOps.unopWithEv[HasIter[A, I], A]
}

object Support {
  implicit def toIterOps[I](lhs: I): HasIterOps[I] = new HasIterOps(lhs)

  type Tagged[U] = { type Tag = U }
  type @@[T, U] = T with Tagged[U]
}

case class SetInt(set: Set[Int]) extends HasIter[Int, Int] {
  import Support._

  trait Tag
  type Iter = Int @@ Tag

  def iter: Iter = set.min.asInstanceOf[Iter]

  def next(i: Int) = set.filter(_ > i).min
  def hasNext(i: Int) = set.exists(_ > i)
  def there(i: Int) = i

  implicit def HasIter: HasIter[Int, Iter] = this.asInstanceOf[HasIter[Int, Iter]]
}


object Test extends App {
  import Support._

  def test1: Unit = {
    val set1 = SetInt(Set(0,2,3,5,6,10))
    import set1.{HasIter => HasIter1}
    val set2 = SetInt(Set(1,2,3,4))
    import set2.{HasIter => HasIter2}

    var it1 = set1.iter
    while (it1.hasNext) {
      println(it1.there)
      it1 = it1.next
    }
    var it2 = set2.iter
    while (it2.hasNext) {
      it2 + 1
      println(it2.there)
      it2 = it2.next
    }
  }
  test1
}
