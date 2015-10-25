package metal

import org.scalatest._

import syntax._

import spire.std.int._

class SetSuite extends FunSuite {

  test("Set operations") {
    val set = MHashSet.empty[String]
    set += "Test"
    assert(set.contains("Test"))
    set += "Test1"
    assert(set.contains("Test1"))
    set -= "Test"
    assert(!set.contains("Test"))
  }

  test("MSet[Int].foreach") {
    val set: MSet[Int] = MHashSet(0,3,5,6)
    var sum = 0
    set.ptr.foreach { p => sum += p.key }
    assert(sum == 14)
  }

  test("Set[Int].count") {
    val set: MSet[Int] = MSortedSet(0,1,2,3,5,6,10)
    assert(set.ptr.count { p => val k = p.key; k > 4 } == 3)
  }

}
