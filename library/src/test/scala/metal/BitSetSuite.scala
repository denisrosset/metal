package metal

import scala.collection.{BitSet => ScalaBitSet}

import org.scalacheck.{Arbitrary, Gen}
import org.scalactic.anyvals.PosZInt

class BitSetSuite extends MetalSuite {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSize = PosZInt(100), sizeRange = PosZInt(30000)).asInstanceOf[PropertyCheckConfiguration]

  def domainSize(a: scala.collection.Set[Int]) =
    if (a.isEmpty) 0 else a.max + 1

  test("mutable.ResizableBitSet &= mutable.ResizableBitSet") {
    forAll { (a1: ScalaBitSet, a2: ScalaBitSet) =>
      val m1 = mutable.ResizableBitSet.fromIterable(a1)
      val m2 = mutable.ResizableBitSet.fromIterable(a2)
      m1 &= m2
      m1.toScala shouldBe (a1 & a2)
    }
  }

  test("mutable.FixedBitSet &= mutable.FixedBitSet") {
    forAll { (a1: ScalaBitSet, a2: ScalaBitSet) =>
      import metal.syntax._
      val m1 = mutable.FixedBitSet.fromIterable(a1)
      val m2 = mutable.FixedBitSet.fromIterable(a2)
      m1 &= m2
      m1.toScala shouldBe (a1 & a2)
    }
  }

  test("mutable.ResizableBitSet &~= mutable.ResizableBitSet") {
    forAll { (a1: ScalaBitSet, a2: ScalaBitSet) =>
      val m1 = mutable.ResizableBitSet.fromIterable(a1)
      val m2 = mutable.ResizableBitSet.fromIterable(a2)
      m1 &~= m2
      m1.toScala shouldBe (a1 &~ a2)
    }
  }

  test("mutable.FixedBitSet &~= mutable.FixedBitSet") {
    forAll { (a1: ScalaBitSet, a2: ScalaBitSet) =>
      val m1 = mutable.FixedBitSet.fromIterable(a1)
      val m2 = mutable.FixedBitSet.fromIterable(a2)
      m1 &~= m2
      m1.toScala shouldBe (a1 &~ a2)
    }
  }


  test("mutable.ResizableBitSet |= mutable.ResizableBitSet") {
    forAll { (a1: ScalaBitSet, a2: ScalaBitSet) =>
      val m1 = mutable.ResizableBitSet.fromIterable(a1)
      val m2 = mutable.ResizableBitSet.fromIterable(a2)
      m1 |= m2
      m1.toScala shouldBe (a1 | a2)
    }
  }

  test("isDisjoint") {
    forAll { (a1: ScalaBitSet, a2: ScalaBitSet) =>
      val i1 = immutable.BitSet.fromIterable(a1)
      val i2 = immutable.BitSet.fromIterable(a2)
      (i1 isDisjoint i2) shouldBe (a1 & a2).isEmpty
    }
  }

  def minOrMinusOne(set: scala.collection.Set[Int]): Int = if (set.isEmpty) -1 else set.min

  test("minOfDifference") {
    forAll { (a1: ScalaBitSet, a2: ScalaBitSet) =>
      val i1 = immutable.BitSet.fromIterable(a1)
      val i2 = immutable.BitSet.fromIterable(a2)
      generic.BitSet.minOfDifference(i1, i2) shouldBe minOrMinusOne(a1 diff a2)
    }
  }

  test("minOfIntersection") {
    forAll { (a1: ScalaBitSet, a2: ScalaBitSet) =>
      val i1 = immutable.BitSet.fromIterable(a1)
      val i2 = immutable.BitSet.fromIterable(a2)
      generic.BitSet.minOfIntersection(i1, i2) shouldBe minOrMinusOne(a1 intersect a2)
    }
  }

  test("nextOfDifference") {
    forAll { (a1: ScalaBitSet, a2: ScalaBitSet, i: Index) =>
      val i1 = immutable.BitSet.fromIterable(a1)
      val i2 = immutable.BitSet.fromIterable(a2)
      generic.BitSet.nextOfDifference(i, i1, i2) shouldBe minOrMinusOne((a1 diff a2).filter(_ > i))
    }
  }

  test("nextOfIntersection") {
    forAll { (a1: ScalaBitSet, a2: ScalaBitSet, i: Index) =>
      val i1 = immutable.BitSet.fromIterable(a1)
      val i2 = immutable.BitSet.fromIterable(a2)
      generic.BitSet.nextOfIntersection(i, i1, i2) shouldBe minOrMinusOne((a1 intersect a2).filter(_ > i))
    }
  }

  test("zeroUntil") {
    forAll { (n: Index) =>
      val i = immutable.BitSet.zeroUntil((n: Int) + 1)
      val b = (0 until (n: Int) + 1).toSet
      i.toScala shouldBe b
    }
  }

  test("apply/update") {
    val bs = mutable.ResizableBitSet(2,4,100)
    bs(2) shouldBe true
    bs(3) shouldBe false
    bs(100) shouldBe true
    bs(100) = false
    bs(101) = true
    bs(200) = true
    bs(100) shouldBe false
    bs(101) shouldBe true
    bs(200) shouldBe false
  }
}

class Index(val underlying: Int) extends AnyVal

object Index {

  implicit def toInt(i: Index): Int = i.underlying

  implicit lazy val arbIndex: Arbitrary[Index] = Arbitrary(
    Gen.sized(sz => Gen.choose(0, sz).map(new Index(_)))
  )

}
