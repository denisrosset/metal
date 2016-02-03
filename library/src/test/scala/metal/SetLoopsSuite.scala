package metal

import org.scalatest._

import syntax._

import spire.std.int._

class SetLoopsSuite extends FunSuite with BeforeAndAfter {

  var sI: MHashSet[Int] = _
  def eI: Enumerable with NElements1[Int] = sI

  before {
    sI = MHashSet(1, 2, 3, 4, 5)
  }

  test("foreach") {
    var sum = 0
    var sumE = 0
    sI.foreach { v =>
      sum += v
    }
    eI.foreach { v =>
      sumE += v
    }
    assert(sum == 15)
    assert(sumE == 15)
  }

  test("count") {
    assert(sI.count(_ > 2) == 3)
    assert(eI.count(_ > 2) == 3)
  }

  test("exists") {
    assert(!sI.exists(_ == 6))
    assert(sI.exists(_ % 2 == 0))
    assert(!eI.exists(_ == 6))
    assert(eI.exists(_ % 2 == 0))
  }

  test("forall") {
    assert(sI.forall(_ > 0))
    assert(!sI.forall(_ > 2))
    assert(eI.forall(_ > 0))
    assert(!eI.forall(_ > 2))
  }

  test("foldLeft") {
    val text1 = ("" /: sI) { (str, el) => str + el.toString }
    val text2 = sI.foldLeft("") { (str, el) => str + el.toString }
    val text3 = ("" /: eI) { (str, el) => str + el.toString }
    val text4 = eI.foldLeft("") { (str, el) => str + el.toString }
    assert(text1.sorted == "12345")
    assert(text2.sorted == "12345")
    assert(text3.sorted == "12345")
    assert(text4.sorted == "12345")
  }

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
  }

}
