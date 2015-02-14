package net.alasc.ptrcoll

trait GenHasPtr[P <: RawPtr] {
  def next(ptr: P): P
  def hasAt(ptr: P): Boolean
}

trait HasPtr[@specialized(Int, Long, Double) A, P <: RawPtr] extends GenHasPtr[P] {
  def at(ptr: P): A
}
