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

trait MapCheck[K, V] extends MetalSuite {

  val factory: metal.mutable.MapFactory
  type MapKV = factory.M[K, V]

  def kName = ctK.runtimeClass.getSimpleName
  def vName = ctV.runtimeClass.getSimpleName
  def collName = factory.getClass.getSimpleName
  override lazy val suiteName = s"MapCheck[$kName, $vName]($collName)"

  implicit def arbK: Arbitrary[K]
  implicit def extraK: factory.KExtra[K]
  implicit def ctK: ClassTag[K]
  implicit def mK: Methods[K]

  implicit def arbV: Arbitrary[V]
  implicit def extraV: factory.VExtra[V]
  implicit def ctV: ClassTag[V]
  implicit def mV: Methods[V]

  def hybridEq(d: MapKV, s: ScalaMap[K, V]): Boolean =
    d.longSize == s.size && s.forall { case (k, v) => d.contains(k) && d(k) == v }

  test("Companion.fromArrays") {
    forAll { (pairs: List[(K, V)]) =>
      val (ks, vs) = pairs.unzip
      val map = factory.fromArrays(ks.toArray, vs.toArray)
      val control = ScalaMap(pairs: _*)
      hybridEq(map, control) shouldBe true
    }
  }

  test("Companion.fromMap") {
    forAll { pairs: List[(K, V)] =>
      val mmap = factory.fromMap(pairs.toMap)
      val control = ScalaMap(pairs: _*)
      hybridEq(mmap, control) shouldBe true
    }
  }

  test("equals (==), hashCode (##)") {
    forAll { (xs: ScalaMap[K, V], ys: ScalaMap[K, V]) =>
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

  test("mutableCopy") {
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

  test("clear") {
    forAll { kvs: List[(K, V)] =>
      val a = factory.fromIterable(kvs)
      a.clear
      a shouldBe factory.empty[K, V]
    }
  }

  test("adding elements (update)") {
    forAll { kvs: ScalaMap[K, V] =>
      val map = factory.empty[K, V]
      val control = ScalaMutableMap.empty[K, V]
      kvs.foreach { case (k, v) =>
        map(k) = v
        control(k) = v
        map.contains(k) shouldBe true
        hybridEq(map, control) shouldBe true
      }
    }
  }

  test("removing elements (remove)") {
    forAll { kvs: ScalaMap[K, V] =>
      val map = factory.fromMap(kvs)
      val control = ScalaMutableMap(kvs.toSeq: _*)
      kvs.foreach { case (k, v) =>
        map.remove(k)
        control -= k
        map.contains(k) shouldBe false
        hybridEq(map, control) shouldBe true
      }
    }
  }

  test("random update and remove") {
    forAll { (pairs: List[(K, V, Boolean)]) =>
      val map = factory.empty[K, V]
      val control = ScalaMutableMap.empty[K, V]
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

  test("pointer iteration") {
    forAll { (kvs: ScalaMap[K, V]) =>
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

  test("forall / exists / findAll") {
    forAll { (kvs: ScalaMap[K, V], f: (K, V) => Boolean) =>
      val m = factory.fromMap(kvs)
      m.forall(f) shouldBe kvs.forall { case (k, v) => f(k, v) }
      m.exists(f) shouldBe kvs.exists { case (k, v) => f(k, v) }
      /*val kvs2 = kvs.filter { case (k, v) => f(k, v) }
      m.findAll(f) shouldBe factory.fromMap(kvs2)*/
    }
  }

}

object MapCheck {

  def apply[K, V](factory0: metal.mutable.MapFactory)(implicit
    arbK0: Arbitrary[K],
    extraK0: factory0.KExtra[K],
    ctK0: ClassTag[K],
    mK0: Methods[K],
    arbV0: Arbitrary[V],
    extraV0: factory0.VExtra[V],
    ctV0: ClassTag[V],
    mV0: Methods[V]): MapCheck[K, V] =
    new MapCheck[K, V] {
      val factory: factory0.type = factory0
      def arbK = arbK0
      def extraK = extraK0
      def ctK = ctK0
      def mK = mK0
      def arbV = arbV0
      def extraV = extraV0
      def ctV = ctV0
      def mV = mV0
    }

}

class MapChecks extends Suites(
  MapCheck[Int, Int](metal.mutable.HashMap),
  MapCheck[Int, Boolean](metal.mutable.HashMap),
  MapCheck[Int, String](metal.mutable.HashMap),
  MapCheck[Long, Int](metal.mutable.HashMap),
  MapCheck[Long, Boolean](metal.mutable.HashMap),
  MapCheck[Long, String](metal.mutable.HashMap),
  MapCheck[String, Int](metal.mutable.HashMap),
  MapCheck[String, Boolean](metal.mutable.HashMap),
  MapCheck[String, String](metal.mutable.HashMap)
)


/*
abstract class MapCheck[K:Arbitrary:ClassTag:Methods, KLB, KExtra[_], V:Arbitrary:ClassTag:Methods, VLB, MP[KK, VV] <: MMap[KK, VV]](factory: MMapFactory[KLB, KExtra, VLB, MP])(implicit kExtra: KExtra[K], klbev: K <:< KLB)
    extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {

  import scala.collection.immutable.Set
  import scala.collection.immutable.Map



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

 */
