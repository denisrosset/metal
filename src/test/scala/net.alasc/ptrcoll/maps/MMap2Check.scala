package net.alasc.ptrcoll
package maps

import org.scalatest.matchers.ShouldMatchers
import org.scalatest._
import prop._
import org.scalacheck.Arbitrary._
import org.scalacheck._
import Gen._
import Arbitrary.arbitrary

import scala.collection.mutable
import scala.reflect._
import scala.{specialized => sp}

import spire.util.Opt

abstract class MMap2Check[K: Arbitrary: ClassTag, KLB, KExtra[_], V1: Arbitrary: ClassTag, VLB1, V2: Arbitrary: ClassTag, VLB2](factory: MMap2Factory[KLB, KExtra, VLB1, VLB2])(implicit kExtra: KExtra[K], klbev: K <:< KLB)
    extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {

  import scala.collection.immutable.Set
  import scala.collection.immutable.Map

  implicit def arbTuple: Arbitrary[(V1, V2)] = Arbitrary {
    for {
      v1 <- implicitly[Arbitrary[V1]].arbitrary
      v2 <- implicitly[Arbitrary[V2]].arbitrary
    } yield (v1, v2)
  }

  def hybridEq(d: MMap2[K, V1, V2], s: mutable.Map[K, (V1, V2)]): Boolean =
    d.size == s.size && s.forall { case (k, (v1, v2)) => d.get1(k) == Opt(v1) && d.get2(k) == Opt(v2) }

  property("fromArrays") {
    forAll { (pairs: List[(K, (V1, V2))]) =>
      val (ks, vs) = pairs.unzip
      val (v1s, v2s) = vs.unzip
      val map = factory.fromArrays(ks.toArray, v1s.toArray, v2s.toArray)
      val control = mutable.Map(pairs: _*)
      hybridEq(map, control) shouldBe true
    }
  }

  property("Companion.fromMap") {
    forAll { pairs: List[(K, (V1, V2))] =>
      val (ks, vs) = pairs.unzip
      val (v1s, v2s) = vs.unzip
      val map = factory.fromMap(pairs.toMap)
      val control = mutable.Map(pairs: _*)
      hybridEq(map, control) shouldBe true
    }
  }

  property("equals (==), hashCode (##)") {
    forAll { (xs: Map[K, (V1, V2)], ys: Map[K, (V1, V2)]) =>
      val a = factory.fromMap(xs)
      val (ks, vs) = xs.unzip
      val (v1s, v2s) = vs.unzip
      val b = factory.fromArrays(ks.toArray.reverse, v1s.toArray.reverse, v2s.toArray.reverse)
      a shouldBe b
      a.## shouldBe b.##

      val c = factory.fromMap(ys)
      if (xs == ys) {
        a shouldBe c
        a.## shouldBe c.##
      } else {
        a should not be c
      }
    }
  }

  property("copy") {
    forAll { kvs: List[(K, (V1, V2))] =>
      val a = factory.fromMap(kvs.toMap)
      val b = a.copy
      a shouldBe b
      kvs.foreach { case (k, _) =>
        a.remove(k)
        a.contains(k) shouldBe false
        b.contains(k) shouldBe true
        a should not be b
      }
    }
  }

  /*

  property("copy") {
    forAll { kvs: List[(A, B)] =>
      val a = DMap.fromIterable(kvs)
      val b = a.copy
      a shouldBe b
      kvs.foreach { case (k, _) =>
        a.remove(k)
        a.contains(k) shouldBe false
        b.contains(k) shouldBe true
        a should not be b
      }
    }
  }

  property("clear") {
    forAll { kvs: List[(A, B)] =>
      val a = DMap.fromIterable(kvs)
      a.clear
      a shouldBe DMap.empty[A, B]
    }
  }
   */

  property("adding elements (update)") {
    forAll { kvs: Map[K, (V1, V2)] =>
      val map = factory.empty[K, V1, V2]
      val control = mutable.Map.empty[K, (V1, V2)]
      kvs.foreach { case (k, (v1, v2)) =>
        map.update(k, v1, v2)
        control(k) = (v1, v2)
        map.contains(k) shouldBe true
        map.get1(k) shouldBe Opt(v1)
        map.get2(k) shouldBe Opt(v2)
        hybridEq(map, control) shouldBe true
      }
    }
  }

  property("removing elements (remove)") {
    forAll { kvs: Map[K, (V1, V2)] =>
      val map = factory.fromMap(kvs)
      val control = mutable.Map(kvs.toSeq: _*)
      kvs.foreach { case (k, (v1, v2)) =>
        map.remove(k)
        control -= k
        map.contains(k) shouldBe false
        hybridEq(map, control) shouldBe true
      }
    }
  }

   property("random += and -=") {
   forAll { (pairs: List[(K, V1, V2, Boolean)]) =>
      val map = factory.empty[K, V1, V2]
      val control = mutable.Map.empty[K, (V1, V2)]
      pairs.foreach {
        case (k, v1, v2, true) => map.update(k, v1, v2); control(k) = (v1, v2)
        case (k, _, _, false) => map.remove(k); control -= k
      }
      hybridEq(map, control) shouldBe true
    }
  }

  /*
  property("foreach") {
    forAll { (kvs: Map[A, B]) =>
      val map1 = DMap.fromIterable(kvs)
      val map2 = DMap.empty[A, B]
      map1.foreach { (k, v) =>
        map2.contains(k) shouldBe false
        map2(k) = v
      }
      map1 shouldBe map2
    }
  }

  property("iterator") {
    forAll { (kvs: Map[A, B]) =>
      val map1 = DMap.fromIterable(kvs)
      val map2 = DMap.empty[A, B]
      map1.iterator.foreach { case (k, v) =>
        map2.contains(k) shouldBe false
        map2(k) = v
      }
      map1 shouldBe map2
    }
  }

  property("mapToSet") {
    forAll { (kvs: Map[A, B], f: (A, B) => B) =>
      val m = DMap.fromIterable(kvs)
      m.mapToSet((a, b) => b) shouldBe DSet.fromArray(m.valuesArray)

      val s2 = kvs.foldLeft(Set.empty[B]) { case (s, (a, b)) =>
        s + f(a, b)
      }
      
      m.mapToSet(f) shouldBe DSet.fromIterable(s2)
    }
  }

  property("mapItemsToMap") {
    forAll { (kvs: Map[A, B], f: (A, B) => (A, B)) =>
      val m = DMap.fromIterable(kvs)
      m.mapToSet((a, b) => b) shouldBe DSet.fromArray(m.valuesArray)

      val kvs2 = kvs.foldLeft(Map.empty[A, B]) { case (m, (a, b)) =>
        val (aa, bb1) = f(a, b)
        val bb2 = m.getOrElse(aa, CMonoid[B].id)
        m.updated(aa, bb1 |+| bb2)
      }
      
      m.mapItemsToMap(f) shouldBe DMap.fromIterable(kvs2)
    }
  }

  property("mapKeys") {
    forAll { (kvs: Map[A, B], f: A => A) =>
      val m = DMap.fromIterable(kvs)
      m.mapKeys(a => a) shouldBe m

      val kvs2 = kvs.foldLeft(Map.empty[A, B]) { case (m, (a, b)) =>
        val aa = f(a)
        val bb = m.getOrElse(aa, CMonoid[B].id)
        m.updated(aa, bb |+| b)
      }

      m.mapKeys(f) shouldBe DMap.fromIterable(kvs2)
    }
  }

  property("mapValues") {
    forAll { (kvs: Map[A, B], f: B => B) =>
      val m = DMap.fromIterable(kvs)
      m.mapValues(b => b) shouldBe m

      m.mapValues(f) shouldBe DMap.fromIterable(kvs.map {
        case (k, v) => (k, f(v))
      })
    }
  }

  property("forall / exists / findAll") {
    forAll { (kvs: Map[A, B], f: (A, B) => Boolean) =>
      val m = DMap.fromIterable(kvs)
      m.forall(f) shouldBe kvs.forall { case (a, b) => f(a, b) }
      m.exists(f) shouldBe kvs.exists { case (a, b) => f(a, b) }

      val kvs2 = kvs.filter { case (a, b) => f(a, b) }
      m.findAll(f) shouldBe DMap.fromIterable(kvs2)
    }
  }
   */
}

class IntIntIntMMap2Check extends MMap2Check[Int, Any, Dummy, Int, Any, Int, Any](HashMMap2)
class IntIntBooleanMap2Check extends MMap2Check[Int, Any, Dummy, Int, Any, Boolean, Any](HashMMap2)
class IntStringIntMap2Check extends MMap2Check[Int, Any, Dummy, String, Any, Int, Any](HashMMap2)

class LongIntIntMMap2Check extends MMap2Check[Long, Any, Dummy, Int, Any, Int, Any](HashMMap2)
class LongIntBooleanMap2Check extends MMap2Check[Long, Any, Dummy, Int, Any, Boolean, Any](HashMMap2)
class LongStringIntMap2Check extends MMap2Check[Long, Any, Dummy, String, Any, Int, Any](HashMMap2)

class StringIntIntMMap2Check extends MMap2Check[String, Any, Dummy, Int, Any, Int, Any](HashMMap2)
class StringIntBooleanMap2Check extends MMap2Check[String, Any, Dummy, Int, Any, Boolean, Any](HashMMap2)
class StringStringIntMap2Check extends MMap2Check[String, Any, Dummy, String, Any, Int, Any](HashMMap2)
