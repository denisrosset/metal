package net.alasc.ptrcoll

import syntax.all._

/** Proof of concept test. */
object Test extends App {
  def test1: Unit = {
    // collection instance
    val set1 = SetInt(Set(0,2,3,5,6,10))
    // necessary import to make the pointer syntax available
    import set1.{PtrTC => PtrTC1}
    val set2 = SetInt(Set(1,2,3,4))
    // renaming the implicit instance is necessary if one wants to point
    // to different collections in the same lexical scope
    import set2.{PtrTC => PtrTC2}

    var ptr1 = set1.pointer
    var ptr2 = set2.pointer
    while (ptr1.hasAt) {
      println(ptr1.at)
      ptr1 = ptr1.next
    }
    while (ptr2.hasAt) {
      println(ptr2.at)
      ptr2 = ptr2.next
    }
  }
  test1
}
