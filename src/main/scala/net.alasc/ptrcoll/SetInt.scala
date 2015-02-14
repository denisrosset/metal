package net.alasc.ptrcoll

case class SetInt(set: Set[Int]) extends Pointable[Int] {
  def pointer: Ptr = if (set.isEmpty) (-1L).asInstanceOf[Ptr] else (set.min.toLong).asInstanceOf[Ptr]

  def next(ptr: Long) = {
    val rest = set.filter(_ > ptr.toInt)
    if (rest.isEmpty) -1L else rest.min.toLong
  }
  def hasAt(ptr: Long) = ptr != -1
  def at(ptr: Long) = if (ptr == -1) Iterator.empty.next else ptr.toInt
}
