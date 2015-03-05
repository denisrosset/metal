package net.alasc.ptrcoll
package sets

import org.scalatest.matchers.ShouldMatchers
import org.scalatest._
import prop._
import org.scalacheck.Arbitrary._
import org.scalacheck._
import Gen._
import Arbitrary.arbitrary

import spire.algebra.Order
import spire.std.any._

import scala.collection.mutable
import scala.reflect._

import syntax.all._

abstract class SSetCheck[A: ClassTag, LB, Extra[_]](factory: SSetFactory[LB, Extra])(implicit extra: Extra[A], lbev: A <:< LB)
    extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {

  implicit def A: Arbitrary[A]

  import scala.collection.immutable.Set

  def hybridEq(d: SSet[A], s: mutable.Set[A]): Boolean =
    d.size == s.size && {
      import d.PtrTC
      var ptr = d.pointer
      var res = true
      while (ptr.hasAt && res) {
        if (!s.contains(ptr.at)) res = false
        ptr = ptr.next
      }
      res
    }

  property("fromArray") {
    forAll { xs: Array[A] =>
      val set = factory.fromArray(xs)
      val control = mutable.Set(xs.toSeq: _*)
      hybridEq(set, control) shouldBe true
    }
  }

  property("Companion.apply") {
    forAll { xs: List[A] =>
//      val set1 = PSet.fromIterable(xs)
      val set2 = factory(xs: _*)
      val control = mutable.Set(xs: _*)
//      hybridEq(set1, control) shouldBe true
      hybridEq(set2, control) shouldBe true
    }
  }

  property("equals (==), hashCode (##)") {
    forAll { xs: List[A] =>
/*      val a = PSet.fromIterable(xs)
 val b = PSet.fromIterable(xs.reverse)*/
      val a = factory(xs: _*)
      val b = factory(xs.reverse: _*)
      a shouldBe b
      a.## shouldBe b.##
    }
  }

  property("!equals (==)") {
    forAll { xs: List[A] =>
      whenever(xs.nonEmpty) {
        val a = factory(xs: _*)
        val b = factory(xs.reverse: _*)
        b -= xs.head
        a should not be b
      }
    }
  }


  property("copy") {
    forAll { xs: List[A] =>
      val a = factory(xs: _*)
      val b = a.copy
      a shouldBe b
      xs.foreach { x =>
        a -= x
        a.contains(x) shouldBe false
        b.contains(x) shouldBe true
        a should not be b
      }
    }
  }
/*
  property("clear") {
    forAll { xs: List[A] =>
      val a = PSet.fromIterable(xs)
      a.clear
      a shouldBe PSet.empty[A]
    }
  }
   */
  property("adding elements (+=)") {
    forAll { xs: Set[A] =>
      val set = factory.empty[A]
      val control = mutable.Set.empty[A]
      xs.foreach { x =>
        set += x
        control += x
        set.contains(x) shouldBe true
        hybridEq(set, control) shouldBe true
      }
    }
  }

  property("removing elements (-=)") {
    forAll { xs: Set[A] =>
      val set = factory(xs.toSeq: _*)
      val control = mutable.Set(xs.toSeq: _*)
      xs.foreach { x =>
        set.contains(x) shouldBe true
        set -= x
        control -= x
        set.contains(x) shouldBe false
        hybridEq(set, control) shouldBe true
      }
    }
  }

  property("random += and -=") {
    forAll { (tpls: List[(A, Boolean)]) =>
      val set = factory.empty[A]
      val control = mutable.Set.empty[A]
      tpls.foreach {
        case (x, true) => set += x; control += x
        case (x, false) => set -= x; control -= x
      }
      hybridEq(set, control) shouldBe true
    }
  }
/*
  property("bulk add (++=)") {
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

  property("union (|)") {
    forAll { (xs: Set[A], ys: Set[A]) =>
      val as = PSet.fromIterable(xs)
      val bs = PSet.fromIterable(ys)
      val cs = PSet.fromIterable(xs | ys)
      (as | bs) shouldBe cs
    }
  }

  property("intersection (&)") {
    forAll { (xs: Set[A], ys: Set[A]) =>
      val as = PSet.fromIterable(xs)
      val bs = PSet.fromIterable(ys)
      val cs = PSet.fromIterable(xs & ys)
      (as & bs) shouldBe cs
    }
  }
  
  property("difference (--)") {
    forAll { (xs: Set[A], ys: Set[A]) =>
      val as = PSet.fromIterable(xs)
      val bs = PSet.fromIterable(ys)
      val cs = PSet.fromIterable(xs -- ys)
      (as -- bs) shouldBe cs
    }
  }

  property("foreach") {
    forAll { (xs: Set[A]) =>
      val a = PSet.fromIterable(xs)
      val b = PSet.empty[A]
      a.foreach { x =>
        b(x) shouldBe false
        b += x
      }
      a shouldBe b
    }
  }

  property("iterator") {
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

  property("map") {
    forAll { (xs: Set[A], f: A => A) =>
      val a = PSet.fromIterable(xs)
      a.map(x => x) shouldBe a
      a.map(f) shouldBe PSet.fromIterable(xs.map(f))
    }
  }

  property("map composition") {
    forAll { (xs: Set[A], f: A => A, g: A => A) =>
      val set = PSet.fromIterable(xs)
      set.map(a => g(f(a))) shouldBe set.map(f).map(g)
    }
  }

  property("partition") {
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

  property("find / exists") {
    forAll { (xs: Set[A], p: A => Boolean) =>
      val a = PSet.fromIterable(xs)
      a.find(p).isDefined shouldBe a.exists(p)
    }
  }

  property("findAll / count") {
    forAll { (xs: Set[A], p: A => Boolean) =>
      val a = PSet.fromIterable(xs)
      a.findAll(p).size shouldBe a.count(p)
    }
  }

  property("findAll / filterSelf") {
    forAll { (xs: Set[A], p: A => Boolean) =>
      val a = PSet.fromIterable(xs)
      val b = PSet.fromIterable(xs)
      a.filterSelf(p)
      a shouldBe b.findAll(p)
    }
  }

  property("toBuffer") {
    forAll { (xs: Array[A]) =>
      val set1 = PSet.fromArray(xs)
      val buf1 = set1.toBuffer
      val buf2 = Buffer.fromArray(xs)
      buf1.sort
      buf2.sort
      //buf1 shouldBe buf2
    }
  }

  property("toMap") {
    forAll { (xs: List[A]) =>
      val set1 = PSet.fromIterable(xs)
      val map1 = set1.toMap(a => a)
      val map2 = DMap.fromIterable(xs.map(a => (a, a)))
      map1 shouldBe map2
    }
  }*/
}

abstract class AutoSSetCheck[A: Arbitrary: ClassTag: Order, LB, Extra[_]](factory: SSetFactory[LB, Extra])(implicit extra: Extra[A], lbev: A <:< LB) extends SSetCheck[A, LB, Extra](factory) {
  def A: Arbitrary[A] = implicitly[Arbitrary[A]]
}

class BooleanSSetCheck extends AutoSSetCheck[Boolean, Any, Dummy](HashSSet)
class IntHashSSetCheck extends AutoSSetCheck[Int, Any, Dummy](HashSSet)
class IntBitSSetCheck extends SSetCheck[Int, Int, Dummy](BitSSet) {
  def A: Arbitrary[Int] = Arbitrary(Gen.choose(0, 10000))
}
class IntSortedSSetCheck extends AutoSSetCheck[Int, Any, Order](SortedSSet)
class StringHashSSetCheck extends AutoSSetCheck[String, Any, Dummy](HashSSet)
class StringSortedSSetCheck extends AutoSSetCheck[String, Any, Order](SortedSSet)
