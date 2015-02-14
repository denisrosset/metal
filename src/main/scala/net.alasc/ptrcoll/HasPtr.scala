package net.alasc.ptrcoll

/** Type class for collection with unboxed pointers (e.g. iterator-like objects). */
trait GenHasPtr[P <: RawPtr] {
  /** Advances the pointer. */
  def next(ptr: P): P
  /** Tests if the pointer points at an object. */
  def hasAt(ptr: P): Boolean
}

trait HasPtr[@specialized(Int, Long, Double) A, P <: RawPtr] extends GenHasPtr[P] {
  /** Returns the object pointed by the pointer. */
  def at(ptr: P): A
}
