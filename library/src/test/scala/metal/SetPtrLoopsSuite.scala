package metal

import org.scalatest._

import syntax._

import spire.std.int._

class SetPtrLoopsSuite extends FunSuite {

  test("foreach") {
    val sI = MHashSet(1,2,3,4,5)
    var sum = 0
    sI.ptr.foreach { vp => sum += vp.key }
    assert(sum == 15)
  }

  test("count") {
    val sI = MHashSet(1,2,3,4,5)
    val c = sI.ptr.count(_.key > 3)
    assert(c == 2)
  }

  test("forall") {
    val sI = MHashSet(1,2,3,4,5)
    val b = sI.ptr.forall(_.key > 0)
    assert(b)
  }

  test("exists") {
    val sI = MHashSet(1,2,3,4,5)
    val b = sI.ptr.exists(_.key == 3)
    assert(b)
  }

  test("foldLeft") {
    val sI = MHashSet(1,2,3,4,5)
    val sm = sI.ptr.foldLeft(0)( _ + _.key )
    val sm1 = (0 /: sI.ptr)( _ + _.key )
    assert(sm == 15)
    assert(sm1 == 15)
  }

  test("maxBy") {
    val sI = MHashSet(1,2,3,4,5)
    val ptr = sI.ptr.maxBy(-_.key)
    assert(ptr.keyOrElse(-1) == 1)
  }

  test("minBy") {
    val sI = MHashSet(1,2,3,4,5)
    val ptr = sI.ptr.minBy(-_.key)
    assert(ptr.keyOrElse(-1) == 5)
  }

  test("sumBy") {
    val sI = MHashSet(1,2,3,4,5)
    val p = sI.ptr.sumBy(-_.key)
    assert(p == -15)
  }

  test("productBy") {
    val sI = MHashSet(1,2,3,4,5)
    val p = sI.ptr.productBy(-_.key)
    assert(p == -120)
  }

}
