package metal

import org.scalatest._
import prop._
import org.scalacheck.Arbitrary._
import org.scalacheck._
import Gen._
import Arbitrary.arbitrary

import scala.collection.mutable
import scala.reflect._
import scala.annotation.tailrec

import spire.util.Opt

import syntax._

abstract class MapCheck[K:Arbitrary:ClassTag:Methods, KLB, KExtra[_], V:Arbitrary:ClassTag:Methods, VLB, MP[KK, VV] <: MMap[KK, VV]](factory: MMapFactory[KLB, KExtra, VLB, MP])(implicit kExtra: KExtra[K], klbev: K <:< KLB)
    extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {

  import scala.collection.immutable.Set
  import scala.collection.immutable.Map

  def hybridEq(d: MP[K, V], s: mutable.Map[K, V]): Boolean =
    d.longSize == s.size && s.forall { case (k, v) => d.contains(k) && d(k) == v }

  property("fromArrays") {
    forAll { (pairs: List[(K, V)]) =>
      val (ks, vs) = pairs.unzip
      val map = factory.fromArrays(ks.toArray, vs.toArray)
      val control = mutable.Map(pairs: _*)
      hybridEq(map, control) shouldBe true
    }
  }

  property("Companion.fromMap") {
    forAll { pairs: List[(K, V)] =>
      val mmap = factory.fromMap(pairs.toMap)
      val control = mutable.Map(pairs: _*)
      hybridEq(mmap, control) shouldBe true
    }
  }

  property("equals (==), hashCode (##)") {
    forAll { (xs: Map[K, V], ys: Map[K, V]) =>
      val a = factory.fromMap(xs)
      val (ks, vs) = xs.unzip
      val b = factory.fromArrays(ks.toArray.reverse, vs.toArray.reverse)
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

  property("mutableCopy") {
    forAll { kvs: List[(K, V)] =>
      val a = factory.fromMap(kvs.toMap)
      val b = a.mutableCopy
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
  property("clear") {
    forAll { kvs: List[(A, B)] =>
      val a = DMap.fromIterable(kvs)
      a.clear
      a shouldBe DMap.empty[A, B]
    }
  }
 */

  property("adding elements (update)") {
    forAll { kvs: Map[K, V] =>
      val map = factory.empty[K, V]
      val control = mutable.Map.empty[K, V]
      kvs.foreach { case (k, v) =>
        map(k) = v
        control(k) = v
        map.contains(k) shouldBe true
        hybridEq(map, control) shouldBe true
      }
    }
  }

  property("removing elements (remove)") {
    forAll { kvs: Map[K, V] =>
      val map = factory.fromMap(kvs)
      val control = mutable.Map(kvs.toSeq: _*)
      kvs.foreach { case (k, v) =>
        map.remove(k)
        control -= k
        map.contains(k) shouldBe false
        hybridEq(map, control) shouldBe true
      }
    }
  }

  property("random update and remove") {
   forAll { (pairs: List[(K, V, Boolean)]) =>
      val map = factory.empty[K, V]
      val control = mutable.Map.empty[K, V]
      pairs.foreach {
        case (k, v, true) =>
          map(k) = v
          control(k) = v
          assert(map(k) == v)
          assert(control(k) == v)
        case (k, _, false) =>
          map.remove(k)
          control.remove(k)
          assert(!map.contains(k))
          assert(!control.contains(k))
      }
      hybridEq(map, control) shouldBe true
    }
  }

  property("pointer iteration") {
    forAll { (kvs: Map[K, V]) =>
      val map1 = factory.fromMap(kvs)
      val map2 = factory.empty[K, V]
      @tailrec def rec(p: Ptr[map1.type]): Unit = p match {
        case IsVPtr(vp) =>
          val k = vp.key
          val v = vp.value
          map2.contains(k) shouldBe false
          map2(k) = v
          rec(vp.next)
        case _ =>
      }
    }
  }

  property("forall / exists / findAll") {
    forAll { (kvs: Map[K, V], f: (K, V) => Boolean) =>
      val m = factory.fromMap(kvs)
      m.forall(f) shouldBe kvs.forall { case (k, v) => f(k, v) }
      m.exists(f) shouldBe kvs.exists { case (k, v) => f(k, v) }
//      val kvs2 = kvs.filter { case (k, v) => f(k, v) }
//      m.findAll(f) shouldBe factory.fromMap(kvs2)
    }
  }

/*
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
   */
}

class IntIntMapCheck extends MapCheck[Int, Any, Dummy, Int, Any, MHashMap](MHashMap)
class IntBooleanMapCheck extends MapCheck[Int, Any, Dummy, Boolean, Any, MHashMap](MHashMap)
class IntStringMapCheck extends MapCheck[Int, Any, Dummy, String, Any, MHashMap](MHashMap)

class LongIntMapCheck extends MapCheck[Long, Any, Dummy, Int, Any, MHashMap](MHashMap)
class LongBooleanMapCheck extends MapCheck[Long, Any, Dummy, Boolean, Any, MHashMap](MHashMap)
class LongStringMapCheck extends MapCheck[Long, Any, Dummy, String, Any, MHashMap](MHashMap)

class StringIntMapCheck extends MapCheck[String, Any, Dummy, Int, Any, MHashMap](MHashMap)
class StringBooleanMapCheck extends MapCheck[String, Any, Dummy, Boolean, Any, MHashMap](MHashMap)
class StringStringMapCheck extends MapCheck[String, Any, Dummy, String, Any, MHashMap](MHashMap)
