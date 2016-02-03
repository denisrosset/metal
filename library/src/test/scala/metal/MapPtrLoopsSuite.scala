package metal

import org.scalatest._

import spire.std.int._

class MapPtrLoopsSuite extends FunSuite {

  test("foreach") {
    val mII = MHashMap(1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8)
    var sumK = 0
    var sumV = 0
    mII.ptr.foreach { vp =>
      sumK += vp.key
      sumV += vp.value
    }
    assert(sumK == 10)
    assert(sumV == 20)
  }

  test("count") {
    val mII = MHashMap(1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8)
    assert(mII.ptr.count { vp => vp.key > 2 } == 2)
    assert(mII.ptr.count { vp => vp.value > 2 } == 3)
  }

  test("exists") {
    val mII = MHashMap(1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8)
    assert(mII.ptr.exists { vp => vp.key > 2 })
    assert(mII.ptr.exists { vp => vp.value > 2 })
    assert(!mII.ptr.exists { vp => vp.key < 1 })
    assert(!mII.ptr.exists { vp => vp.value < 1 })
  }

  test("forall") {
    val mII = MHashMap(1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8)
    assert(mII.ptr.forall { vp => 2 * vp.key == vp.value })
    assert(!mII.ptr.forall { vp => 3 * vp.key == vp.value + 1 })
  }

  test("foldLeft") {
    val mII = MHashMap(1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8)
    val text1 = ("" /: mII.ptr) { (str, vp) => str + vp.key.toString + vp.value.toString }
    val text2 = mII.ptr.foldLeft("") { (str, vp) => str + vp.key.toString + vp.value.toString }
    assert(text1.sorted == "12234468")
    assert(text2.sorted == "12234468")
  }

  test("minBy") {
    val mII = MHashMap(1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8)
    val minPtr = mII.ptr.minBy(_.key)
    minPtr match {
      case IsVPtr(vp) =>
        val k = vp.key
        assert(k == 1)
      case _ => assert(false)
    }
  }

  test("max") {
    val mII = MHashMap(1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8)
    val maxPtr = mII.ptr.maxBy(_.value)
    maxPtr match {
      case IsVPtr(vp) =>
        val k = vp.value
        assert(k == 8)
      case _ => assert(false)
    }
  }

  test("sumBy") {
    val mII = MHashMap(1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8)
    val res = mII.ptr.sumBy(_.value)
    assert(res == 20)
  }

  test("product") {
    val mII = MHashMap(1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8)
    val res = mII.ptr.productBy(_.key)
    assert(res == 24)
  }

}
