package metal

import org.scalatest._

class Map2PtrOpsSuite extends FunSuite {

  def testMap2 = MHashMap2(1 -> ((2, 3)))

  test("remove") {
    val m2 = testMap2
    m2.ptr match {
      case IsVPtr(vp) =>
        vp.remove
        assert(m2.isEmpty)
    }
  }

  test("key") {
    val m2 = testMap2
    m2.ptrFind(1) match {
      case IsVPtr(vp) =>
        assert(vp.key == 1)
      case _ => assert(false)
    }
  }

  test("value1") {
    val m2 = testMap2
    m2.ptrFind(1) match {
      case IsVPtr(vp) =>
        assert(vp.value1 == 2)
      case _ => assert(false)
    }
  }

  test("value2") {
    val m2 = testMap2
    m2.ptrFind(1) match {
      case IsVPtr(vp) =>
        assert(vp.value2 == 3)
      case _ => assert(false)
    }
  }

  test("element1") {
    val m2 = testMap2
    m2.ptrFind(1) match {
      case IsVPtr(vp) =>
        val e1 = vp.element1
        assert(e1 == 1)
      case _ => assert(false)
    }
  }

  test("element2") {
    val m2 = testMap2
    m2.ptrFind(1) match {
      case IsVPtr(vp) =>
        val e2 = vp.element2
        assert(e2 == 2)
      case _ => assert(false)
    }
  }

  test("element3") {
    val m2 = testMap2
    m2.ptrFind(1) match {
      case IsVPtr(vp) =>
        val e2 = vp.element3
        assert(e2 == 3)
      case _ => assert(false)
    }
  }

  test("keyOrElse") {
    val m2 = testMap2
    assert(m2.ptrFind(1).keyOrElse(-1) == 1)
    assert(m2.ptrFind(3).keyOrElse(-1) == -1)
  }

  test("value1OrElse") {
    val m2 = testMap2
    assert(m2.ptrFind(1).value1OrElse(-1) == 2)
    assert(m2.ptrFind(3).value1OrElse(-1) == -1)
  }

  test("value2OrElse") {
    val m2 = testMap2
    assert(m2.ptrFind(1).value2OrElse(-1) == 3)
    assert(m2.ptrFind(3).value2OrElse(-1) == -1)
  }

  test("element1OrElse") {
    val m2 = testMap2
    assert(m2.ptrFind(1).element1OrElse(-1) == 1)
    assert(m2.ptrFind(3).element1OrElse(-1) == -1)
  }

  test("element2OrElse") {
    val m2 = testMap2
    assert(m2.ptrFind(1).element2OrElse(-1) == 2)
    assert(m2.ptrFind(3).element2OrElse(-1) == -1)
  }

}
