package metal

import org.scalatest._

import syntax._

class MapOpsSuite extends FunSuite {

  def testMap = MHashMap(1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8)

  // SearchableOps
  test("contains") {
    val mII = testMap
    assert(mII.contains(3))
    assert(!mII.contains(6))
  }

  // RemovableSearchableOps
  test("remove") {
    val mII = testMap
    assert(mII.remove(2))
    assert(mII.contains(3))
    assert(!mII.contains(2))
  }

  test("-=") {
    val mII = testMap
    assert((mII -= 2) eq mII)
    assert(mII.contains(3))
    assert(!mII.contains(2))
  }

  // UpdateOps
  test("update") {
    val mII = testMap
    mII(5) = 4
    assert(mII.contains(5))
    assert(mII(5) == 4)
  }

  // SearchableValuesOps
  test("containsItem") {
    val mII = testMap
    assert(mII.containsItem(2, 4))
    assert(!mII.containsItem(2, 5))
    assert(!mII.containsItem(3, 4))
  }

  test("apply") {
    val mII = testMap
    assert(mII(1) == 2)
    assert(mII(2) == 4)
    assert(mII(3) == 6)
    assert(mII(4) == 8)
    intercept[NoSuchElementException] {
      mII(5)
    }
  }

  test("getOrElse") {
    val mII = testMap
    assert(mII.getOrElse(1, -1) == 2)
    assert(mII.getOrElse(6, -1) == -1)
  }

  test("get") {
    val mII = testMap
    assert(mII.get(6).isEmpty)
    assert(mII.get(1).isDefined)
    assert(mII.get(1).get == 2)
  }

}
