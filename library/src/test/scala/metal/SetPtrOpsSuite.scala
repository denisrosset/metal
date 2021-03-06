package metal

import spire.std.int._

import metal.mutable.{HashSet => MHashSet, ArraySortedSet => MSortedSet}

class SetPtrOpsSuite extends MetalSuite {

  test("next") {
    val sI = MHashSet(1)
    val p = sI.ptr
    assert(p.nonNull)
    p match {
      case IsVPtr(vp) => assert(vp.next.isNull)
      case _ =>
    }
  }

  test("remove") {
    val sI = MHashSet(1)
    sI.ptr match {
      case IsVPtr(vp) =>
        vp.remove
        assert(sI.isEmpty)
    }
  }

  test("removeAndAdvance") {
    val sI = MSortedSet(1, 2)
    sI.ptr.get.key
    sI.ptr match {
      case IsVPtr(vp) =>
        val k = vp.key
        assert(k == 1)
        vp.removeAndAdvance match {
          case IsVPtr(vp1) =>
            val k1 = vp1.key
            assert(k1 == 2)
          case _ => assert(false)
        }
      case _ => assert(false)
    }
  }

  test("key") {
    val sI = MHashSet(2, 4)
    sI.ptrFind(2) match {
      case IsVPtr(vp) =>
        val k = vp.key
        assert(k == 2)
      case _ => assert(false)
    }
  }

  test("keyOrElse") {
    val sI = MHashSet(2, 4)
    val k1 = sI.ptrFind(2).keyOrElse(-1)
    assert(k1 == 2)
    val k2 = sI.ptrFind(3).keyOrElse(-1)
    assert(k2 == -1)
  }

    test("element") {
    val sI = MHashSet(2, 4)
    sI.ptrFind(2) match {
      case IsVPtr(vp) =>
        val e = vp.element
        assert(e == 2)
      case _ => assert(false)
    }
  }

  test("elementOrElse") {
    val sI = MHashSet(2, 4)
    val e1 = sI.ptrFind(2).elementOrElse(-1)
    assert(e1 == 2)
    val e2 = sI.ptrFind(3).elementOrElse(-1)
    assert(e2 == -1)
  }

}
