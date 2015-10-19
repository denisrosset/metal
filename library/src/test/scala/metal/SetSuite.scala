package metal

import org.scalatest._

import syntax._

import spire.std.int._

class SetSuite extends FunSuite {

  test("Set operations") {
    val set = HashSet.empty[String]
    set += "Test"
    assert(set.contains("Test"))
    set += "Test1"
    assert(set.contains("Test1"))
    set -= "Test"
    assert(!set.contains("Test"))
  }

  test("Set[Int].foreach") {
    val set: Set[Int] = HashSet(0,3,5,6)
    var sum = 0
    set.foreach { i => sum += i }
    assert(sum == 14)
  }

  test("Set[Int].count") {
    val set: Set[Int] = SortedSet(0,1,2,3,5,6,10)
    assert(set.count(_ > 4) == 3)
  }

}
