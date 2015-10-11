package net.alasc.ptrcoll

class Ptr[T <: Pointable#Tag] protected[ptrcoll] (val v: Long) extends AnyVal {
  @inline final def isNull = v == -1L
  @inline final def nonNull = !isNull

  // for name-based extractors
  @inline final def isEmpty = isNull
  @inline final def get: VPtr[T] = new VPtr[T](v)
}

object Ptr {
  implicit def fromVPtr[T <: Pointable#Tag](vPtr: VPtr[T]): Ptr[T] = new Ptr[T](vPtr.v)
  @inline final def Null[T <: Pointable#Tag]: Ptr[T] = new Ptr[T](-1L)
}

class VPtr[T <: Pointable#Tag] protected[ptrcoll] (val v: Long) extends AnyVal {
  @inline final def isNull: Boolean = false
  @inline final def nonNull: Boolean = true

  @inline final def next: Ptr[T] = macro Macros.next[T]
  @inline final def key[A]: A = macro Macros.key[T, A]
//  @inline final def value = macro Macros.value[T]
//  @inline final def value1 = macro Macros.value1[T]
//  @inline final def value2 = macro Macros.value2[T]
}

object VPtr {
  @inline final def apply[T <: Pointable#Tag](v: Long): VPtr[T] = new VPtr[T](v)
  @inline final def unapply[T <: Pointable#Tag](ptr: Ptr[T]): Ptr[T] = ptr
}
