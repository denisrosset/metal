package net.alasc.ptrcoll

import sets._
import spire.syntax.cfor._

/** Proof of concept test. */
object Test extends App {
  import spire.std.int.IntAlgebra
  def test1: Unit = {
    // collection instance
    val set1 = SortedSSet(0,2,3,5,6,10)
    val set2 = SortedSSet(1,2,3,4)

    var ptr1 = set1.ptrStart
    var ptr2 = set2.ptrStart
    cforRange(0 until 1000000) { i => set1.ptrStart.valid }
    ptr1.valid
/*    while (ptr1.hasAt) {
      println(ptr1.at)
      ptr1 = ptr1.nextPtr
    }
    while (ptr2.hasAt) {
      println(ptr2.at)
      ptr2 = ptr2.nextPtr
    }*/
  }/*
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
  test2*/
}
