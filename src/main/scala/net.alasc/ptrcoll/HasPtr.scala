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

trait HasPtrVal[@specialized(Int, Long) V, P <: RawPtr] extends HasPtr[P] {
  /** Returns the value of the object pointed by the pointer. */
  def atVal(ptr: P): V
}

trait HasPtrVal1[V1, P <: RawPtr] extends HasPtr[P] {
  /** Returns the value of the object pointed by the pointer. */
  def atVal1(ptr: P): V1
}

trait HasPtrVal2[V2, P <: RawPtr] extends HasPtr[P] {
  /** Returns the value of the object pointed by the pointer. */
  def atVal2(ptr: P): V2
}
