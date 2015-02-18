package net.alasc.ptrcoll

/** Type class for collection with unboxed pointers (e.g. iterator-like objects). */
trait HasPtr[P <: RawPtr] {
  /** Advances the pointer. */
  def next(ptr: P): P
  /** Tests if the pointer points at an object. */
  def hasAt(ptr: P): Boolean
}

trait HasPtrAt[@specialized(Int) A, P <: RawPtr] extends HasPtr[P] {
  /** Returns the object pointed by the pointer. */
  def at(ptr: P): A
}

/*
trait HasPtrKey[@specialized(Int, Long) K, P <: RawPtr] extends HasPtr[P] {
  /** Returns the key of the object pointed by the pointer. */
  def atKey(ptr: P): K
}

trait HasPtrVal[@specialized(Int, Long) V, P <: RawPtr] extends HasPtr[P] {
  def atVal(ptr: P): V
}
 */
