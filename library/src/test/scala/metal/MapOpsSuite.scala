package metal

import org.scalatest._

import syntax._

import spire.std.int._

class MapOpsSuite extends FunSuite {

  // SearchableOps
  test("contains") {
    val mII = MHashMap(1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8)
    assert(mII.contains(3))
    assert(!mII.contains(6))
  }

  // RemovableSearchableOps
  test("remove") {
    val mII = MHashMap(1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8)
    assert(mII.remove(2))
    assert(mII.contains(3))
    assert(!mII.contains(2))
  }

  test("-=") {
    val mII = MHashMap(1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8)
    assert((mII -= 2) eq mII)
    assert(mII.contains(3))
    assert(!mII.contains(2))
  }

  // UpdateOps
  test("update") {
    val mII = MHashMap(1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8)
    mII(5) = 4
    assert(mII.contains(5))
    assert(mII(5) == 4)
  }

  // SearchableValuesOps
  test("containsItem") {
    val mII = MHashMap(1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8)
    assert(mII.containsItem(2, 4))
    assert(!mII.containsItem(2, 5))
    assert(!mII.containsItem(3, 4))
  }

  test("apply") {
    val mII = MHashMap(1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8)
    assert(mII(1) == 2)
    assert(mII(2) == 4)
    assert(mII(3) == 6)
    assert(mII(4) == 8)
    intercept[NoSuchElementException] {
      mII(5)
    }
    assert(mII.get(6).isEmpty)
    assert(mII.get(1).isDefined)
    assert(mII.get(1).get == 2)
  }

  test("getOrElse") {
    val mII = MHashMap(1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8)
    assert(mII.getOrElse(1, -1) == 2)
    assert(mII.getOrElse(6, -1) == -1)
  }

  test("get") {
    val mII = MHashMap(1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8)
    assert(mII.get(6).isEmpty)
    assert(mII.get(1).isDefined)
    assert(mII.get(1).get == 2)
  }

}
