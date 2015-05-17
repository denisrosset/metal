package net.alasc.ptrcoll

sealed trait Validity
sealed trait IsValid extends Validity

class RawPtr[T <: Pointable#Tag, +V <: Validity] protected[ptrcoll] (val v: Long) extends AnyVal {
  @inline final def valid: RawPtr[T, IsValid] = new RawPtr[T, IsValid](v)
  @inline final def isNull = v == -1L
  @inline final def nonNull = !isNull
  // for name-based extractors
  @inline final def isEmpty = isNull
  @inline final def get: RawPtr[T, IsValid] = this.asInstanceOf[RawPtr[T, IsValid]]
}

object ValidPtr {
  @inline final def apply[T <: Pointable#Tag](v: Long): RawPtr[T, IsValid] = new RawPtr[T, IsValid](v)
}

object NullPtr {
  @inline final def apply[T <: Pointable#Tag]: RawPtr[T, Validity] = new RawPtr[T, Validity](-1L)
  @inline final def apply(p: Pointable): RawPtr[p.Tag, Validity] = new RawPtr[p.Tag, Validity](-1L)
}

object Valid {
  @inline final def unapply[T <: Pointable#Tag, V <: Validity](ptr: RawPtr[T, V]): RawPtr[T, V] = ptr
}
