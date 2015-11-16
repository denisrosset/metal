package metal

import org.scalatest._

import syntax._

import spire.std.int._

class MapLoopsSuite extends FunSuite with BeforeAndAfter {

  var mII: MHashMap[Int, Int] = _
  def eII: Enumerable with Elements[(Int, Int)] = mII

  before {
    mII = MHashMap(1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8)
  }

  test("foreach") {
    var sumK = 0
    var sumV = 0
    var sumKE = 0
    var sumVE = 0
    mII.foreach { (k, v) =>
      sumK += k
      sumV += v
    }
    eII.foreach { pair =>
      sumKE += pair._1
      sumVE += pair._2
    }
    assert(sumK == 10)
    assert(sumV == 20)
    assert(sumKE == 10)
    assert(sumVE == 20)
  }

  test("count") {
    assert(mII.count { (k, v) => k > 2 } == 2)
    assert(mII.count { (k, v) => v > 2 } == 3)
    assert(eII.count { pair => pair._1 > 2 } == 2)
    assert(eII.count { pair => pair._2 > 2 } == 3)
  }

  test("exists") {
    assert(mII.exists { (k, v) => k > 2 })
    assert(mII.exists { (k, v) => v > 2 })
    assert(eII.exists { pair => pair._1 > 2 })
    assert(eII.exists { pair => pair._2 > 2 })
    assert(!mII.exists { (k, v) => k < 1 })
    assert(!mII.exists { (k, v) => v < 1 })
    assert(!eII.exists { pair => pair._1 < 1 })
    assert(!eII.exists { pair => pair._2 < 1 })
  }

  test("forall") {
    assert(mII.forall { (k, v) => 2 * k == v })
    assert(!mII.forall { (k, v) => 3 * k == v + 1 })
    assert(eII.forall { pair => 2 * pair._1 == pair._2 })
    assert(!eII.forall { pair => 3 * pair._1 == pair._2 + 1 })
  }

  test("foldLeft") {
    val text1 = ("" /: mII) { (str, k, v) => str + k.toString + v.toString }
    val text2 = mII.foldLeft("") { (str, k, v) => str + k.toString + v.toString }
    val text3 = ("" /: eII) { (str, pair) => str + pair._1.toString + pair._2.toString }
    val text4 = eII.foldLeft("") { (str, pair) => str + pair._1.toString + pair._2.toString }
    assert(text1.sorted == "12234468")
    assert(text2.sorted == "12234468")
    assert(text3.sorted == "12234468")
    assert(text4.sorted == "12234468")
  }

/*

  test("min") {
    assert(sI.min == 1)
    assert(eI.min == 1)
  }

  test("max") {
    assert(sI.max == 5)
    assert(eI.max == 5)
  }

  test("sum") {
    assert(sI.sum == 15)
    assert(eI.sum == 15)
  }

  test("product") {
    assert(sI.product == 120)
    assert(eI.product == 120)
  }*/

}
