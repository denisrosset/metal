package net.alasc.ptrcoll

import syntax.all._
import sets._

/** Proof of concept test. */
object Test extends App {
  import spire.std.int.IntAlgebra
  def test1: Unit = {
    // collection instance
    val set1 = SortedSSet(0,2,3,5,6,10)
    // necessary import to make the pointer syntax available
    import set1.{PtrTC => PtrTC1}
    val set2 = SortedSSet(1,2,3,4)
    // renaming the implicit instance is necessary if one wants to point
    // to different collections in the same lexical scope
    import set2.{PtrTC => PtrTC2}

    var ptr1 = set1.pointer
    var ptr2 = set2.pointer
    while (ptr1.hasAt) {
      println(ptr1.at)
      ptr1 = ptr1.nextPtr
    }
    while (ptr2.hasAt) {
      println(ptr2.at)
      ptr2 = ptr2.nextPtr
    }
  }
  def test2: Unit = {
    val set = BitSSet.empty[Int]
    var i = 0
    while (i < 100000) {
      set += i
      i += 1
    }
    var loop = 0
    while (loop < 1000) {
      var sum = 0
      import set.PtrTC
      var ptr = set.pointer
      while (ptr.hasAt) {
        sum += ptr.at
        ptr = ptr.nextPtr
      }
      loop += 1
    }
  }
  test1
  test2
}
