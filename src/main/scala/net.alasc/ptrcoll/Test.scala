package net.alasc.ptrcoll

import Syntax._

object Test extends App {
  def test1: Unit = {
    val set1 = SetInt(Set(0,2,3,5,6,10))
    import set1.{HasPtr => HasPtr1}
    val set2 = SetInt(Set(1,2,3,4))
    import set2.{HasPtr => HasPtr2}

    var ptr1 = set1.pointer
    while (ptr1.hasAt) {
      println(ptr1.at)
      ptr1 = ptr1.next
    }
    var ptr2 = set2.pointer
    while (ptr2.hasAt) {
      println(ptr2.at)
      ptr2 = ptr2.next
    }
  }
  test1
}
