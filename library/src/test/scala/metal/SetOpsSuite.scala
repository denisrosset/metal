package metal

import org.scalatest._

import syntax._

import spire.std.int._

class SetOpsSuite extends FunSuite with BeforeAndAfter {

  test("contains") {
    val sI = MHashSet(1, 2, 3, 4, 5)
    assert(sI.contains(3))
    assert(!sI.contains(6))
  }

  test("remove") {
    val sI = MHashSet(1, 2, 3, 4, 5)
    assert(sI.remove(2))
    assert(sI.contains(3))
    assert(!sI.contains(2))
  }

  test("-=") {
    val sI = MHashSet(1, 2, 3, 4, 5)
    assert((sI -= 2) eq sI)
    assert(sI.contains(3))
    assert(!sI.contains(2))
  }

  test("add") {
    val sI = MHashSet(1, 2, 3)
    assert(sI.add(2))
    assert(!sI.add(6))
    assert(sI.contains(2))
    assert(sI.contains(6))
  }

  test("+=") {
    val sI = MHashSet(1, 2, 3)
    assert((sI += 6) eq sI)
    assert(sI.contains(6))
  }
  
}
