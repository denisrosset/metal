package metal

import spire.algebra._

/** (Possibly null) pointer on a container element. Uses a path-dependent tag type
  * to link the pointer to its container (Pointable#Tag).
  */
class Ptr[T <: Pointable#Tag](val v: Long) extends AnyVal {

  override def toString = s"Ptr($v)"
  @inline final def isNull = v == -1L
  @inline final def nonNull = v != -1L

  final def keyOrElse[A](orElse: A): A = macro macros.PtrOps.keyOrElse[T, A]
  final def valueOrElse[A](orElse: A): A = macro macros.PtrOps.valueOrElse[T, A]
  final def value1OrElse[A](orElse: A): A = macro macros.PtrOps.value1OrElse[T, A]
  final def value2OrElse[A](orElse: A): A = macro macros.PtrOps.value2OrElse[T, A]

  def foreach(body: VPtr[T] => Unit): Unit = macro macros.PtrLoops.foreach[T]
  def count(body: VPtr[T] => Boolean): Int = macro macros.PtrLoops.count[T]
  def exists(body: VPtr[T] => Boolean): Boolean = macro macros.PtrLoops.exists[T]
  def forall(body: VPtr[T] => Boolean): Boolean = macro macros.PtrLoops.forall[T]
  def foldLeft[A](z: A)(body: (A, VPtr[T]) => A): A = macro macros.PtrLoops.foldLeft[T, A]
  def /:[A](z: A)(body: (A, VPtr[T]) => A): A = macro macros.PtrLoops.foldLeft[T, A]

  def minBy[A](body: VPtr[T] => A)(implicit orderA: Order[A]): Ptr[T] = macro macros.PtrLoops.minBy[T, A]
  def maxBy[A](body: VPtr[T] => A)(implicit orderA: Order[A]): Ptr[T] = macro macros.PtrLoops.maxBy[T, A]

  def sumBy[A](body: VPtr[T] => A)(implicit am: AdditiveMonoid[A]): A = macro macros.PtrLoops.sumBy[T, A]
  def productBy[A](body: VPtr[T] => A)(implicit mm: MultiplicativeMonoid[A]): A = macro macros.PtrLoops.productBy[T, A]

  // for name-based extractors
  @inline final def isEmpty = isNull
  @inline final def get: VPtr[T] = new VPtr[T](v)

}

object Ptr {

  implicit def fromVPtr[T <: Pointable#Tag](vPtr: VPtr[T]): Ptr[T] = new Ptr[T](vPtr.v)
  @inline final def apply[T <: Pointable#Tag](v: Long): Ptr[T] = new Ptr[T](v)
  @inline final def Null[T <: Pointable#Tag]: Ptr[T] = new Ptr[T](-1L)

}

/** Non-null pointer on a container element. */
class VPtr[T <: Pointable#Tag](val v: Long) extends AnyVal {

  override def toString = s"VPtr($v)"
  @inline final def isNull: Boolean = false
  @inline final def nonNull: Boolean = true

  final def next: Ptr[T] = macro macros.PtrOps.next[T]
  final def remove: Unit = macro macros.PtrOps.remove[T]
  final def removeAndAdvance: Ptr[T] = macro macros.PtrOps.removeAndAdvance[T]

  final def key[A]: A = macro macros.PtrOps.key[T, A]
  final def value[A]: A = macro macros.PtrOps.value[T, A]
  final def update[A](newValue: A): Unit = macro macros.PtrOps.update[T, A]
  final def value1[A]: A = macro macros.PtrOps.value1[T, A]
  final def value2[A]: A = macro macros.PtrOps.value2[T, A]
  final def update1[A](newValue: A): Unit = macro macros.PtrOps.update1[T, A]
  final def update2[A](newValue: A): Unit = macro macros.PtrOps.update2[T, A]

}

object VPtr {

  @inline final def apply[T <: Pointable#Tag](v: Long): VPtr[T] = new VPtr[T](v)
  @inline final def unapply[T <: Pointable#Tag](ptr: Ptr[T]): Ptr[T] = ptr

}
