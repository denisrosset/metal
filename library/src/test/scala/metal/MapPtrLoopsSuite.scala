package metal

import org.scalatest._

import syntax._

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
    assert(mII.ptr.count { vp => val k = vp.key; k > 2 } == 2)
    assert(mII.ptr.count { vp => val v = vp.value; v > 2 } == 3)
  }

  test("exists") {
    val mII = MHashMap(1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8)
    assert(mII.ptr.exists { vp => val k = vp.key; k > 2 })
    assert(mII.ptr.exists { vp => val v = vp.value; v > 2 })
    assert(!mII.ptr.exists { vp => val k = vp.key; k < 1 })
    assert(!mII.ptr.exists { vp => val v = vp.value; v < 1 })
  }

  test("forall") {
    val mII = MHashMap(1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8)
    assert(mII.ptr.forall { vp => val k = vp.key; val v = vp.value; 2 * k == v })
    assert(!mII.ptr.forall { vp => val k = vp.key; val v = vp.value; 3 * k == v + 1 })
  }

  test("foldLeft") {
    val mII = MHashMap(1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8)
    val text1 = ("" /: mII.ptr) { (str, vp) => val k = vp.key; val v = vp.value; str + k.toString + v.toString }
    val text2 = mII.ptr.foldLeft("") { (str, vp) => val k = vp.key; val v = vp.value; str + k.toString + v.toString }
    assert(text1.sorted == "12234468")
    assert(text2.sorted == "12234468")
  }

  test("minBy") {
    val mII = MHashMap(1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8)
    val minPtr = mII.ptr.minBy { vp => val k = vp.key; k }
    minPtr match {
      case VPtr(vp) =>
        val k = vp.key
        assert(k == 1)
      case _ => assert(false)
    }
  }

  test("max") {
    val mII = MHashMap(1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8)
    val maxPtr = mII.ptr.maxBy { vp => val v = vp.value; v }
    maxPtr match {
      case VPtr(vp) =>
        val k = vp.value
        assert(k == 8)
      case _ => assert(false)
    }
  }

  test("sumBy") {
    val mII = MHashMap(1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8)
    val res = mII.ptr.sumBy { vp => val v = vp.value; v }
    assert(res == 20)
  }

  test("product") {
    val mII = MHashMap(1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8)
    val res = mII.ptr.productBy { vp => val k = vp.key; k }
    assert(res == 24)
  }

}
