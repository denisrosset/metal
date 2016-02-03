package metal

import org.scalatest._

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

  test("element1") {
    val mII = MHashMap(1 -> 2)
    mII.ptrFind(1) match {
      case IsVPtr(vp) =>
        val e1 = vp.element1
        assert(e1 == 1)
      case _ => assert(false)
    }
  }

  test("element2") {
    val mII = MHashMap(1 -> 2)
    mII.ptrFind(1) match {
      case IsVPtr(vp) =>
        val e2 = vp.element2
        assert(e2 == 2)
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
    val v1 = mII.ptrFind(1).valueOrElse(-1)
    assert(v1 == 2)
    val v2 = mII.ptrFind(3).valueOrElse(-1)
    assert(v2 == -1)
  }

  test("element1OrElse") {
    val mII = MHashMap(1 -> 2)
    val e1 = mII.ptrFind(1).element1OrElse(-1)
    assert(e1 == 1)
    val e2 = mII.ptrFind(3).element1OrElse(-1)
    assert(e2 == -1)
  }

  test("element2OrElse") {
    val mII = MHashMap(1 -> 2)
    val e1 = mII.ptrFind(1).element2OrElse(-1)
    assert(e1 == 2)
    val e2 = mII.ptrFind(3).element2OrElse(-1)
    assert(e2 == -1)
  }

}
