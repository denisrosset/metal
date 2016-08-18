package metal

import scala.collection.{BitSet => ScalaBitSet}

import org.scalactic.anyvals.PosZInt

class BitSetSuite extends MetalSuite {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSize = PosZInt(100), sizeRange = PosZInt(30000)).asInstanceOf[PropertyCheckConfiguration]

  def domainSize(a: scala.collection.Set[Int]) =
    if (a.isEmpty) 0 else a.max + 1

  test("mutable.BitSet &= mutable.BitSet") {
    forAll { (a1: ScalaBitSet, a2: ScalaBitSet) =>
      val m1 = mutable.BitSet.fromIterable(a1)
      val m2 = mutable.BitSet.fromIterable(a2)
      m1 &= m2
      m1.toScala shouldBe (a1 & a2)
    }
  }

  test("mutable.BitSet &= mutable.BitSet (fixed size)") {

    forAll { (a1: ScalaBitSet, a2: ScalaBitSet) =>
      import metal.syntax._
      val m1 = mutable.BitSet.fixedSize(domainSize(a1))
      a1.foreach( m1 += _ )
      val m2 = mutable.BitSet.fixedSize(domainSize(a2))
      a2.foreach( m2 += _)
      m1 &= m2
      m1.toScala shouldBe (a1 & a2)
    }
  }

  test("mutable.BitSet &= mutable.HashSet[Int]") {
    forAll { (a1: ScalaBitSet, a2: ScalaBitSet) =>
      val m1 = mutable.BitSet.fromIterable(a1)
      val m2 = mutable.HashSet.fromIterable(a2)
      m1 &= m2
      m1.toScala shouldBe (a1 & a2)
    }
  }

  test("mutable.BitSet &= mutable.HashSet[Int] (fixed size)") {
    forAll { (a1: ScalaBitSet, a2: ScalaBitSet) =>
      import metal.syntax._
      val m1 = mutable.BitSet.fixedSize(domainSize(a1))
      a1.foreach( m1 += _ )
      val m2 = mutable.HashSet.fromIterable(a2)
      m1 &= m2
      m1.toScala shouldBe (a1 & a2)
    }
  }

  test("mutable.BitSet &~= mutable.BitSet") {
    forAll { (a1: ScalaBitSet, a2: ScalaBitSet) =>
      val m1 = mutable.BitSet.fromIterable(a1)
      val m2 = mutable.BitSet.fromIterable(a2)
      m1 &~= m2
      m1.toScala shouldBe (a1 &~ a2)
    }
  }

  test("mutable.BitSet &~= mutable.HashSet") {
    forAll { (a1: ScalaBitSet, a2: ScalaBitSet) =>
      val m1 = mutable.BitSet.fromIterable(a1)
      val m2 = mutable.HashSet.fromIterable(a2)
      m1 &~= m2
      m1.toScala shouldBe (a1 &~ a2)
    }
  }

  test("mutable.BitSet |= mutable.BitSet") {
    forAll { (a1: ScalaBitSet, a2: ScalaBitSet) =>
      val m1 = mutable.BitSet.fromIterable(a1)
      val m2 = mutable.BitSet.fromIterable(a2)
      m1 |= m2
      m1.toScala shouldBe (a1 | a2)
    }
  }

  test("mutable.BitSet |= mutable.HashSet") {
    forAll { (a1: ScalaBitSet, a2: ScalaBitSet) =>
      val m1 = mutable.BitSet.fromIterable(a1)
      val m2 = mutable.HashSet.fromIterable(a2)
      m1 |= m2
      m1.toScala shouldBe (a1 | a2)
    }
  }

  test("Discovered bug in mutable.BitSet &=") {
    val arg0 = mutable.BitSet(0, 2, 3, 4, 5, 9, 10, 13, 15, 17, 18, 19, 25, 27, 28, 29, 33, 35, 38, 39, 41, 43, 46, 47, 49, 50, 51, 54, 57, 60, 62, 63, 64, 66, 67, 69)
    val arg1 = mutable.BitSet.empty
    arg0 &= arg1
    arg0.isEmpty shouldBe true
  }

}
