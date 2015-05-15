package net.alasc.ptrcoll

/** Type class for collection with unboxed pointers (e.g. iterator-like objects). */
@annotation.implicitNotFound("Typeclass not found for pointer of type ${P}. Import the typeclass from the pointed collection using `import coll.{PtrTC => collPtrTC}`.")
trait HasPtr[P <: RawPtr] {
  /** Advances the pointer. */
  def nextPtr(ptr: P): P
  /** Tests if the pointer points at an object. */
  def hasAt(ptr: P): Boolean
}

object HasPtr {
  implicit def Ambiguous1: HasPtr[RawPtr] = null
  implicit def Ambiguous2: HasPtr[RawPtr] = null
}

@annotation.implicitNotFound("Typeclass not found for pointer of type ${P}. Import the typeclass from the pointed collection using `import coll.{PtrTC => collPtrTC}`.")
trait HasPtrAt[@specialized(Int) K, P <: RawPtr] extends HasPtr[P] {
  /** Returns the object pointed by the pointer. */
  def at(ptr: P): K
}

object HasPtrAt {
  implicit def Ambiguous1[K]: HasPtrAt[K, RawPtr] = null
  implicit def Ambiguous2[K]: HasPtrAt[K, RawPtr] = null
}

@annotation.implicitNotFound("Typeclass not found for pointer of type ${P}. Import the typeclass from the pointed collection using `import coll.{PtrTC => collPtrTC}`.")
trait HasPtrVal[@specialized(Int, Long) V, P <: RawPtr] extends HasPtr[P] {
  /** Returns the value of the object pointed by the pointer. */
  def atVal(ptr: P): V
}

object HasPtrVal {
  implicit def Ambiguous1[V]: HasPtrVal[V, RawPtr] = null
  implicit def Ambiguous2[V]: HasPtrVal[V, RawPtr] = null
}

@annotation.implicitNotFound("Typeclass not found for pointer of type ${P}. Import the typeclass from the pointed collection using `import coll.{PtrTC => collPtrTC}`.")
trait HasPtrVal1[V1, P <: RawPtr] extends HasPtr[P] {
  /** Returns the value of the object pointed by the pointer. */
  def atVal1(ptr: P): V1
}

object HasPtrVal1 {
  implicit def Ambiguous1[V1]: HasPtrVal1[V1, RawPtr] = null
  implicit def Ambiguous2[V1]: HasPtrVal1[V1, RawPtr] = null
}

@annotation.implicitNotFound("Typeclass not found for pointer of type ${P}. Import the typeclass from the pointed collection using `import coll.{PtrTC => collPtrTC}`.")
trait HasPtrVal2[V2, P <: RawPtr] extends HasPtr[P] {
  /** Returns the value of the object pointed by the pointer. */
  def atVal2(ptr: P): V2
}

object HasPtrVal2 {
  implicit def Ambiguous1[V2]: HasPtrVal[V2, RawPtr] = null
  implicit def Ambiguous2[V2]: HasPtrVal[V2, RawPtr] = null
}
