package metal

import scala.annotation.tailrec
import scala.collection.{Set => ScalaSet}
import scala.collection.mutable.{Set => ScalaMutableSet}
import scala.reflect.ClassTag

import spire.std.any._

import org.scalatest.Suites
import org.scalacheck.{Arbitrary, Gen}

import metal.syntax._

import generic.Methods

trait SetCheck[A] extends MetalSuite {

  type SetA <: metal.mutable.Set[A]

  def hybridEq(d: SetA, s: ScalaSet[A]): Boolean =
    d.longSize == s.size && {
      @tailrec def rec(p: Ptr[d.type]): Boolean = p match {
        case IsVPtr(vp) =>
          if (s.contains(d.ptrKey(vp)))
            rec(d.ptrNext(vp))
          else
            false
        case _ => true
      }
      rec(d.ptr)
    }

  def collName: String
  def aName = ctA.runtimeClass.getSimpleName
  override lazy val suiteName = s"SetCheck[$aName]($collName)"

  implicit def arbA: Arbitrary[A]
  implicit def ctA: ClassTag[A]
  implicit def mA: Methods[A]

  def emptySet: SetA
  def fromIterable(xs: Iterable[A]): SetA
  def fromArray(xs: Array[A]): SetA
  def apply(xs: A*): SetA

  test("Companion.fromIterable") {
    forAll { xs: Iterable[A] =>
      val set = fromIterable(xs)
      val control = ScalaSet(xs.toSeq: _*)
      hybridEq(set, control) shouldBe true
    }
  }

  test("Companion.apply") {
    forAll { xs: List[A] =>
      val set = apply(xs: _*)
      val control = ScalaSet(xs: _*)
      hybridEq(set, control) shouldBe true
    }
  }
  test("Companion.fromArray") {
    forAll { xs: Array[A] =>
      val set = fromArray(xs)
      val control = ScalaSet(xs.toSeq: _*)
      hybridEq(set, control) shouldBe true
    }
  }

  test("equals (==), hashCode (##)") {
    forAll { xs: List[A] =>
      val a = apply(xs: _*)
      val b = apply(xs.reverse: _*)
      a shouldBe b
      a.## shouldBe b.##
    }
  }

  test("!equals (==)") {
    forAll { xs: List[A] =>
      whenever(xs.nonEmpty) {
        val a = apply(xs: _*)
        val b = apply(xs.reverse: _*)
        b -= xs.head
        a should not be b
      }
    }
  }

  test("copy") {
    forAll { xs: List[A] =>
      val a = apply(xs: _*)
      val b = a.mutableCopy
      a shouldBe b
      xs.foreach { x =>
        a -= x
        a.contains(x) shouldBe false
        b.contains(x) shouldBe true
        a should not be b
      }
    }
  }

  test("clear") {
    forAll { xs: List[A] =>
      val a = fromIterable(xs)
      a.clear
      a shouldBe emptySet
    }
  }

  test("adding elements (+=)") {
    forAll { xs: ScalaSet[A] =>
      val set = emptySet
      val control = ScalaMutableSet.empty[A]
      xs.foreach { x =>
        set += x
        control += x
        set.contains(x) shouldBe true
        hybridEq(set, control) shouldBe true
      }
    }
  }

  test("removing elements (-=)") {
    forAll { xs: ScalaSet[A] =>
      val set = apply(xs.toSeq: _*)
      val control = ScalaMutableSet(xs.toSeq: _*)
      xs.foreach { x =>
        set.contains(x) shouldBe true
        set -= x
        control -= x
        set.contains(x) shouldBe false
        hybridEq(set, control) shouldBe true
      }
    }
  }

  test("random += and -=") {
    forAll { (tpls: List[(A, Boolean)]) =>
      val set = emptySet
      val control = ScalaMutableSet.empty[A]
      tpls.foreach {
        case (x, true) => set += x; control += x
        case (x, false) => set -= x; control -= x
      }
      hybridEq(set, control) shouldBe true
    }
  }

  test("foreach") {
    forAll { (xs: ScalaSet[A]) =>
      val a = apply(xs.toSeq: _*)
      val b = emptySet
      a.foreach { x =>
        b.contains(x) shouldBe false
        b += x
      }
      a shouldBe b
    }
  }

  test("find / exists") {
    forAll { (xs: ScalaSet[A], p: A => Boolean) =>
      val a = apply(xs.toSeq: _*)
      xs.find(p).isDefined shouldBe a.exists(p)
    }
  }

}

trait FactorySetCheck[A] extends SetCheck[A] {

  val factory: metal.mutable.SetFactory
  type SetA = factory.S[A]
  implicit def extra: factory.Extra[A]
  def collName = factory.getClass.getSimpleName

  def emptySet: SetA = factory.empty[A]
  def fromIterable(xs: Iterable[A]): SetA = factory.fromIterable(xs)
  def fromArray(xs: Array[A]): SetA = factory.fromArray(xs)
  def apply(xs: A*): SetA = factory(xs: _*)

}

object FactorySetCheck {

  def apply[A](factory0: metal.mutable.SetFactory)(implicit
    arbA0: Arbitrary[A],
    ctA0: ClassTag[A],
    extra0: factory0.Extra[A],
    mA0: Methods[A]
  ): SetCheck[A] =
    new FactorySetCheck[A] {
      val factory: factory0.type = factory0
      def arbA = arbA0
      def ctA = ctA0
      def mA = mA0
      def extra = extra0
    }

}

class BitSetCheck(implicit val ctA: ClassTag[Int], val mA: Methods[Int]) extends SetCheck[Int] {

  def arbA: Arbitrary[Int] = Arbitrary(Gen.choose(0, 10000))

  type SetA = metal.mutable.BitSet
  def collName = "BitSet"
  def emptySet = metal.mutable.BitSet.empty
  def apply(xs: Int*) = metal.mutable.BitSet(xs: _*)
  def fromArray(xs: Array[Int]) = metal.mutable.BitSet.fromArray(xs)
  def fromIterable(xs: Iterable[Int]) = metal.mutable.BitSet.fromIterable(xs)

}

class SetChecks extends Suites(
  new BitSetCheck,
  FactorySetCheck[Int](metal.mutable.HashSet),
  FactorySetCheck[Int](metal.mutable.ArraySortedSet),
  FactorySetCheck[Boolean](metal.mutable.HashSet),
  FactorySetCheck[Boolean](metal.mutable.ArraySortedSet),
  FactorySetCheck[String](metal.mutable.HashSet),
  FactorySetCheck[String](metal.mutable.ArraySortedSet)
)

/*
abstract class SetCheck[A:Arbitrary:ClassTag:Methods, Extra[_], ST[X] <: metal.mutable.Set[X]](factory: metal.mutable.SetFactory.Aux[Extra, ST])(implicit extra: Extra[A]) {



/*
  test("bulk add (++=)") {
    forAll { (xs: List[A], ys: List[A]) =>
      val set = PSet.empty[A]
      val control = mutable.Set.empty[A]

      set ++= xs
      control ++= xs
      hybridEq(set, control) shouldBe true

      set ++= ys
      control ++= ys
      hybridEq(set, control) shouldBe true
    }
  }

  test("union (|)") {
    forAll { (xs: Set[A], ys: Set[A]) =>
      val as = PSet.fromIterable(xs)
      val bs = PSet.fromIterable(ys)
      val cs = PSet.fromIterable(xs | ys)
      (as | bs) shouldBe cs
    }
  }

  test("intersection (&)") {
    forAll { (xs: Set[A], ys: Set[A]) =>
      val as = PSet.fromIterable(xs)
      val bs = PSet.fromIterable(ys)
      val cs = PSet.fromIterable(xs & ys)
      (as & bs) shouldBe cs
    }
  }
  
  test("difference (--)") {
    forAll { (xs: Set[A], ys: Set[A]) =>
      val as = PSet.fromIterable(xs)
      val bs = PSet.fromIterable(ys)
      val cs = PSet.fromIterable(xs -- ys)
      (as -- bs) shouldBe cs
    }
  }

  test("iterator") {
    forAll { (xs: Set[A]) =>
      val a = PSet.fromIterable(xs)
      val b = PSet.empty[A]
      a.iterator.foreach { x =>
        b(x) shouldBe false
        b += x
      }
      a shouldBe b
    }
  }

  test("map") {
    forAll { (xs: Set[A], f: A => A) =>
      val a = PSet.fromIterable(xs)
      a.map(x => x) shouldBe a
      a.map(f) shouldBe PSet.fromIterable(xs.map(f))
    }
  }

  test("map composition") {
    forAll { (xs: Set[A], f: A => A, g: A => A) =>
      val set = PSet.fromIterable(xs)
      set.map(a => g(f(a))) shouldBe set.map(f).map(g)
    }
  }

  test("partition") {
    forAll { (xs: Set[A], f: A => Boolean) =>
      val a = PSet.fromIterable(xs)
      val (b, c) = a.partition(f)
      b.foreach { x => a(x) shouldBe true }
      c.foreach { x => a(x) shouldBe true }
      a.size shouldBe (b.size + c.size)
      b.exists(f) shouldBe false
      c.forall(f) shouldBe true
    }
  }


  test("findAll / count") {
    forAll { (xs: Set[A], p: A => Boolean) =>
      val a = PSet.fromIterable(xs)
      a.findAll(p).size shouldBe a.count(p)
    }
  }

  test("findAll / filterSelf") {
    forAll { (xs: Set[A], p: A => Boolean) =>
      val a = PSet.fromIterable(xs)
      val b = PSet.fromIterable(xs)
      a.filterSelf(p)
      a shouldBe b.findAll(p)
    }
  }

  test("toBuffer") {
    forAll { (xs: Array[A]) =>
      val set1 = PSet.fromArray(xs)
      val buf1 = set1.toBuffer
      val buf2 = Buffer.fromArray(xs)
      buf1.sort
      buf2.sort
      //buf1 shouldBe buf2
    }
  }

  test("toMap") {
    forAll { (xs: List[A]) =>
      val set1 = PSet.fromIterable(xs)
      val map1 = set1.toMap(a => a)
      val map2 = DMap.fromIterable(xs.map(a => (a, a)))
      map1 shouldBe map2
    }
  }*/

}

 */
