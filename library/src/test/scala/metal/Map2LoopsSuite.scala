package metal

import org.scalatest._

import syntax._

import spire.std.int._

class Map2LoopsSuite extends FunSuite {

  def testMap2: MHashMap2[Int, Int, Int] = MHashMap2(1 -> (2, 3), 4 -> (5, 6), 7 -> (8, 9))

  test("foreach") {
    val m = testMap2
    var sumK = 0
    var sumV1 = 0
    var sumV2 = 0
    m.foreach { (k, v1, v2) =>
      sumK += k
      sumV1 += v1
      sumV2 += v2
    }
    assert(sumK == 12)
    assert(sumV1 == 15)
    assert(sumV2 == 18)
  }

  test("count") {
    val m = testMap2
    assert(m.count { (k, v1, v2) => k > 2 } == 2)
    assert(m.count { (k, v1, v2) => v1 > 2 } == 2)
    assert(m.count { (k, v1, v2) => v2 > 2 } == 3)
  }

  test("exists") {
    val m = testMap2
    assert(m.exists { (k, v1, v2) => k > 2 })
    assert(m.exists { (k, v1, v2) => v1 > 2 })
    assert(m.exists { (k, v1, v2) => v2 > 2 })
    assert(!m.exists { (k, v1, v2) => k < 1 })
    assert(!m.exists { (k, v1, v2) => v1 < 1 })
    assert(!m.exists { (k, v1, v2) => v2 < 1 })
  }

  test("forall") {
    val m = testMap2
    assert(m.forall { (k, v1, v2) => k + 1 == v1 })
    assert(!m.forall { (k, v1, v2) => k * 2 == v1 })
  }

  test("foldLeft") {
    val m = testMap2
    val text1 = ("" /: m) { (str, k, v1, v2) => str + k.toString + v1.toString + v2.toString }
    val text2 = m.foldLeft("") { (str, k, v1, v2) => str + k.toString + v1.toString + v2.toString }
    assert(text1.sorted == "123456789")
    assert(text2.sorted == "123456789")
  }

}
