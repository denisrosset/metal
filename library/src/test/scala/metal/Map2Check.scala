package metal

import scala.annotation.tailrec
import scala.collection.{Map => ScalaMap}
import scala.collection.mutable.{Map => ScalaMutableMap}
import scala.reflect.ClassTag

import spire.std.any._
import spire.util.Opt

import org.scalatest.Suites
import org.scalacheck.{Arbitrary, Gen}

import metal.syntax._

import generic.Methods

trait Map2Check[K, V1, V2] extends MetalSuite {

  val factory: metal.mutable.Map2Factory
  type MapKV1V2 = factory.M[K, V1, V2]

  def kName = ctK.runtimeClass.getSimpleName
  def v1Name = ctV1.runtimeClass.getSimpleName
  def v2Name = ctV2.runtimeClass.getSimpleName
  def collName = factory.getClass.getSimpleName
  override lazy val suiteName = s"Map2Check[$kName, $v1Name, $v2Name]($collName)"

  implicit def arbK: Arbitrary[K]
  implicit def extraK: factory.KExtra[K]
  implicit def ctK: ClassTag[K]
  implicit def mK: Methods[K]

  implicit def arbV1: Arbitrary[V1]
  implicit def extraV1: factory.V1Extra[V1]
  implicit def ctV1: ClassTag[V1]
  implicit def mV1: Methods[V1]

  implicit def arbV2: Arbitrary[V2]
  implicit def extraV2: factory.V2Extra[V2]
  implicit def ctV2: ClassTag[V2]
  implicit def mV2: Methods[V2]

  implicit def arbTuple: Arbitrary[(V1, V2)] = Arbitrary {
    for {
      v1 <- implicitly[Arbitrary[V1]].arbitrary
      v2 <- implicitly[Arbitrary[V2]].arbitrary
    } yield (v1, v2)
  }

  def hybridEq(d: MapKV1V2, s: ScalaMap[K, (V1, V2)]): Boolean =
    d.longSize == s.size && s.forall { case (k, (v1, v2)) => d.contains(k) && d.apply1(k) == v1 && d.apply2(k) == v2 }

  test("fromArrays") {
    forAll { (pairs: List[(K, (V1, V2))]) =>
      val (ks, vs) = pairs.unzip
      val (v1s, v2s) = vs.unzip
      val map = factory.fromArrays(ks.toArray, v1s.toArray, v2s.toArray)
      val control = ScalaMap(pairs: _*)
      hybridEq(map, control) shouldBe true
    }
  }

  test("Companion.fromMap") {
    forAll { pairs: List[(K, (V1, V2))] =>
      val (ks, vs) = pairs.unzip
      val (v1s, v2s) = vs.unzip
      val map = factory.fromMap(pairs.toMap)
      val control = ScalaMap(pairs: _*)
      hybridEq(map, control) shouldBe true
    }
  }

  test("equals (==), hashCode (##)") {
    forAll { (xs: ScalaMap[K, (V1, V2)], ys: ScalaMap[K, (V1, V2)]) =>
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

  test("mutableCopy") {
    forAll { kvs: List[(K, (V1, V2))] =>
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

  test("pointer iteration") {
    forAll { (kvs: ScalaMap[K, (V1, V2)]) =>
      val map1 = factory.fromMap(kvs)
      val map2 = factory.empty[K, V1, V2]
      @tailrec def rec(p: Ptr[map1.type]): Unit = p match {
        case IsVPtr(vp) =>
          val k = vp.key
          val v1 = vp.value1
          val v2 = vp.value2
          map2.contains(k) shouldBe false
          map2(k) = (v1, v2)
          rec(map1.ptrNext(vp))
        case _ =>
      }
      rec(map1.ptr)
      map1.longSize shouldBe kvs.size
      map2.longSize shouldBe kvs.size
      map1 shouldBe map2
    }
  }

  test("copy") {
    forAll { kvs: List[(K, (V1, V2))] =>
      val a = factory.fromIterable(kvs)
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

  test("clear") {
    forAll { kvs: List[(K, (V1, V2))] =>
      val a = factory.fromIterable(kvs)
      a.clear
      a shouldBe factory.empty[K, V1, V2]
    }
  }

  test("adding elements (update)") {
    forAll { kvs: ScalaMap[K, (V1, V2)] =>
      val map = factory.empty[K, V1, V2]
      val control = ScalaMutableMap.empty[K, (V1, V2)]
      kvs.foreach { case (k, (v1, v2)) =>
        map(k) = (v1, v2)
        control(k) = (v1, v2)
        map.contains(k) shouldBe true
        map.get1(k) shouldBe Opt(v1)
        map.get2(k) shouldBe Opt(v2)
        hybridEq(map, control) shouldBe true
      }
    }
  }

  test("removing elements (remove)") {
    forAll { kvs: ScalaMap[K, (V1, V2)] =>
      val map = factory.fromMap(kvs)
      val control = ScalaMutableMap(kvs.toSeq: _*)
      kvs.foreach { case (k, (v1, v2)) =>
        map.remove(k)
        control -= k
        map.contains(k) shouldBe false
        hybridEq(map, control) shouldBe true
      }
    }
  }

  test("random += and -=") {
    forAll { (pairs: List[(K, V1, V2, Boolean)]) =>
      val map = factory.empty[K, V1, V2]
      val control = ScalaMutableMap.empty[K, (V1, V2)]
      pairs.foreach {
        case (k, v1, v2, true) => map(k) = (v1, v2); control(k) = (v1, v2)
        case (k, _, _, false) => map.remove(k); control -= k
      }
      hybridEq(map, control) shouldBe true
    }
  }

  test("foreach") {
    forAll { (kvs: ScalaMap[K, (V1, V2)]) =>
      val map1 = factory.fromMap(kvs)
      val map2 = factory.empty[K, V1, V2]
      map1.foreach { (k, v1, v2) =>
        map2.contains(k) shouldBe false
        map2(k) = (v1, v2)
      }
      map1 shouldBe map2
    }
  }

  test("forall / exists / findAll") {
    forAll { (kvs: ScalaMap[K, (V1, V2)], f: (K, V1, V2) => Boolean) =>
      val m = factory.fromMap(kvs)
      m.forall(f) shouldBe kvs.forall { case (k, (v1, v2)) => f(k, v1, v2) }
      m.exists(f) shouldBe kvs.exists { case (k, (v1, v2)) => f(k, v1, v2) }
      /*      val kvs2 = kvs.filter { case (k, v) => f(k, v) }
       m.findAll(f) shouldBe factory.fromMap(kvs2)*/
    }
  }

}

object Map2Check {

  def apply[K, V1, V2](factory0: metal.mutable.Map2Factory)(implicit
    arbK0: Arbitrary[K],
    extraK0: factory0.KExtra[K],
    ctK0: ClassTag[K],
    mK0: Methods[K],
    arbV10: Arbitrary[V1],
    extraV10: factory0.V1Extra[V1],
    ctV10: ClassTag[V1],
    mV10: Methods[V1],
    arbV20: Arbitrary[V2],
    extraV20: factory0.V2Extra[V2],
    ctV20: ClassTag[V2],
    mV20: Methods[V2]): Map2Check[K, V1, V2] =
    new Map2Check[K, V1, V2] {
      val factory: factory0.type = factory0
      def arbK = arbK0
      def extraK = extraK0
      def ctK = ctK0
      def mK = mK0
      def arbV1 = arbV10
      def extraV1 = extraV10
      def ctV1 = ctV10
      def mV1 = mV10
      def arbV2 = arbV20
      def extraV2 = extraV20
      def ctV2 = ctV20
      def mV2 = mV20
    }

}

class Map2Checks extends Suites(
  Map2Check[Int, Int, Int](metal.mutable.HashMap2),
  Map2Check[Int, Int, Boolean](metal.mutable.HashMap2),
  Map2Check[Int, Int, String](metal.mutable.HashMap2),
  Map2Check[Long, Int, Int](metal.mutable.HashMap2),
  Map2Check[Long, Int, Boolean](metal.mutable.HashMap2),
  Map2Check[String, Int, Int](metal.mutable.HashMap2),
  Map2Check[String, Int, Boolean](metal.mutable.HashMap2),
  Map2Check[String, String, Int](metal.mutable.HashMap2)
)

/*
test("iterator") {
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

test("mapToSet") {
  forAll { (kvs: Map[A, B], f: (A, B) => B) =>
    val m = DMap.fromIterable(kvs)
    m.mapToSet((a, b) => b) shouldBe DSet.fromArray(m.valuesArray)

    val s2 = kvs.foldLeft(Set.empty[B]) { case (s, (a, b)) =>
      s + f(a, b)
    }
    
    m.mapToSet(f) shouldBe DSet.fromIterable(s2)
  }
}

test("mapItemsToMap") {
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

test("mapKeys") {
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

test("mapValues") {
  forAll { (kvs: Map[A, B], f: B => B) =>
    val m = DMap.fromIterable(kvs)
    m.mapValues(b => b) shouldBe m

    m.mapValues(f) shouldBe DMap.fromIterable(kvs.map {
      case (k, v) => (k, f(v))
    })
  }
}

 */
