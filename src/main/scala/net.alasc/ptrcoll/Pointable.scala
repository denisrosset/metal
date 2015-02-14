package net.alasc.ptrcoll

trait Pointable[@specialized(Int, Long, Double) A] extends HasPtr[A, RawPtr] { self =>
  trait Tag
  type Ptr = TaggedPtr[Tag]
  def pointer: Ptr
  def next(ptr: RawPtr): RawPtr
  def hasAt(ptr: RawPtr): Boolean
  def at(ptr: RawPtr): A
  implicit def HasPtr: HasPtr[A, Ptr] = self.asInstanceOf[HasPtr[A, Ptr]]
}
