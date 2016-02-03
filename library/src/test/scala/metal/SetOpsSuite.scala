package metal

import org.scalatest._

import syntax._

class SetOpsSuite extends FunSuite {

  // SearchableOps
  test("contains") {
    val sI = MHashSet(1, 2, 3, 4, 5)
    assert(sI.contains(3))
    assert(!sI.contains(6))
  }

  // RemovableSearchableOps
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

  // AddOps
  test("+=") {
    val sI = MHashSet(1, 2, 3)
    assert((sI += 6) eq sI)
    assert(sI.contains(6))
  }

  // SearchableAddOps
  test("add") {
    val sI = MHashSet(1, 2, 3)
    assert(sI.add(2))
    assert(!sI.add(6))
    assert(sI.contains(2))
    assert(sI.contains(6))
  }
  
}
