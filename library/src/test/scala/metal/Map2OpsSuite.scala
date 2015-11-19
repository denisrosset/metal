package metal

import org.scalatest._

import syntax._

import spire.std.int._

class Map2OpsSuite extends FunSuite {

  def testMap2 = MHashMap2(1 -> (2, 3), 4 -> (5, 6))

  // SearchableOps
  test("contains") {
    val m = testMap2
    assert(m.contains(1))
    assert(!m.contains(6))
  }

  // RemovableSearchableOps
  test("remove") {
    val m = testMap2
    assert(m.remove(1))
    assert(m.contains(4))
    assert(!m.contains(1))
  }

  test("-=") {
    val m = testMap2
    assert((m -= 1) eq m)
    assert(m.contains(4))
    assert(!m.contains(1))
  }

  // Update2Ops
  test("update") {
    val m = testMap2
    m(7) = (8, 9)
    assert(m.contains(7))
    assert(m.apply1(7) == 8)
    assert(m.apply2(7) == 9)
  }

  // SearchableValues12Ops
  test("containsItem2") {
    val m = testMap2
    assert(m.containsItem2(1, 2, 3))
    assert(!m.containsItem2(1, 2, 4))
    assert(!m.containsItem2(3, 4, 5))
  }

  // SearchableValues1Ops

  test("apply1") {
    val m = testMap2
    assert(m.apply1(1) == 2)
    assert(m.apply1(4) == 5)
    intercept[NoSuchElementException] {
      m.apply1(5)
    }
  }

  test("getOrElse1") {
    val m = testMap2
    assert(m.getOrElse1(1, -1) == 2)
    assert(m.getOrElse1(6, -1) == -1)
  }

  test("get1") {
    val m = testMap2
    assert(m.get1(6).isEmpty)
    assert(m.get1(1).isDefined)
    assert(m.get1(1).get == 2)
  }

  // SearchableValues2Ops

  test("apply2") {
    val m = testMap2
    assert(m.apply2(1) == 3)
    assert(m.apply2(4) == 6)
    intercept[NoSuchElementException] {
      m.apply2(5)
    }
  }

  test("getOrElse2") {
    val m = testMap2
    assert(m.getOrElse2(1, -1) == 3)
    assert(m.getOrElse2(6, -1) == -1)
  }

  test("get2") {
    val m = testMap2
    assert(m.get2(6).isEmpty)
    assert(m.get2(1).isDefined)
    assert(m.get2(1).get == 3)
  }

}
