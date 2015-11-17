package metal

import org.scalatest._

import syntax._

import spire.std.int._

class MapPtrOpsSuite extends FunSuite {

  test("remove") {
    val mII = MHashMap(1 -> 2)
    mII.ptr match {
      case IsVPtr(vp) =>
        vp.remove
        assert(mII.isEmpty)
    }
  }

  test("key") {
    val mII = MHashMap(1 -> 2)
    mII.ptrFind(1) match {
      case IsVPtr(vp) =>
        val k = vp.key
        assert(k == 1)
      case _ => assert(false)
    }
  }

  test("value") {
    val mII = MHashMap(1 -> 2)
    mII.ptrFind(1) match {
      case IsVPtr(vp) =>
        val v = vp.value
        assert(v == 2)
      case _ => assert(false)
    }
  }

  test("keyOrElse") {
    val mII = MHashMap(1 -> 2)
    val k1 = mII.ptrFind(1).keyOrElse(-1)
    assert(k1 == 1)
    val k2 = mII.ptrFind(3).keyOrElse(-1)
    assert(k2 == -1)
  }

  test("valueOrElse") {
    val mII = MHashMap(1 -> 2)
    val k1 = mII.ptrFind(1).valueOrElse(-1)
    assert(k1 == 2)
    val k2 = mII.ptrFind(3).valueOrElse(-1)
    assert(k2 == -1)
  }

}
