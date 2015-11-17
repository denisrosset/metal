package metal

import org.scalatest._

import syntax._

import spire.std.int._

class SetPtrOpsSuite extends FunSuite {

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

}
