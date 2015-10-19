package metal

import spire.algebra._

class Ptr[T <: Pointable#Tag](val v: Long) extends AnyVal {

  override def toString = s"Ptr($v)"
  @inline final def isNull = v == -1L
  @inline final def nonNull = !isNull

  final def keyOrElse[A](orElse: A): A = macro PtrMacros.keyOrElse[T, A]
  final def valueOrElse[A](orElse: A): A = macro PtrMacros.valueOrElse[T, A]
  final def value1OrElse[A](orElse: A): A = macro PtrMacros.value1OrElse[T, A]
  final def value2OrElse[A](orElse: A): A = macro PtrMacros.value2OrElse[T, A]

  // for name-based extractors
  @inline final def isEmpty = isNull
  @inline final def get: VPtr[T] = new VPtr[T](v)

  def foreach(body: VPtr[T] => Unit): Unit = macro PtrMacros.foreach[T]
  def count(body: VPtr[T] => Boolean): Int = macro PtrMacros.count[T]
  def exists(body: VPtr[T] => Boolean): Boolean = macro PtrMacros.exists[T]
  def forall(body: VPtr[T] => Boolean): Boolean = macro PtrMacros.forall[T]
  def foldLeft[A](z: A)(body: (A, VPtr[T]) => A): A = macro PtrMacros.foldLeft[T, A]
  def /:[A](z: A)(body: (A, VPtr[T]) => A): A = macro PtrMacros.foldLeft[T, A]

  def minBy[A](body: VPtr[T] => A)(implicit orderA: Order[A]): Ptr[T] = macro PtrMacros.minBy[T, A]
  def maxBy[A](body: VPtr[T] => A)(implicit orderA: Order[A]): Ptr[T] = macro PtrMacros.maxBy[T, A]

  def sumBy[A](body: VPtr[T] => A)(implicit am: AdditiveMonoid[A]): A = macro PtrMacros.sumBy[T, A]
  def productBy[A](body: VPtr[T] => A)(implicit mm: MultiplicativeMonoid[A]): A = macro PtrMacros.productBy[T, A]

}

object Ptr {

  implicit def fromVPtr[T <: Pointable#Tag](vPtr: VPtr[T]): Ptr[T] = new Ptr[T](vPtr.v)
  @inline final def apply[T <: Pointable#Tag](v: Long): Ptr[T] = new Ptr[T](v)
  @inline final def Null[T <: Pointable#Tag]: Ptr[T] = new Ptr[T](-1L)

}

class VPtr[T <: Pointable#Tag](val v: Long) extends AnyVal {

  override def toString = s"VPtr($v)"
  @inline final def isNull: Boolean = false
  @inline final def nonNull: Boolean = true

  final def next: Ptr[T] = macro PtrMacros.next[T]
  final def remove: Unit = macro PtrMacros.remove[T]
  final def removeAndAdvance: Ptr[T] = macro PtrMacros.removeAndAdvance[T]

  final def key[A]: A = macro PtrMacros.key[T, A]
  final def value[A]: A = macro PtrMacros.value[T, A]
  final def update[A](newValue: A): Unit = macro PtrMacros.update[T, A]
  final def value1[A]: A = macro PtrMacros.value1[T, A]
  final def value2[A]: A = macro PtrMacros.value2[T, A]
  final def update1[A](newValue: A): Unit = macro PtrMacros.update1[T, A]
  final def update2[A](newValue: A): Unit = macro PtrMacros.update2[T, A]

}

object VPtr {

  @inline final def apply[T <: Pointable#Tag](v: Long): VPtr[T] = new VPtr[T](v)
  @inline final def unapply[T <: Pointable#Tag](ptr: Ptr[T]): Ptr[T] = ptr

}
