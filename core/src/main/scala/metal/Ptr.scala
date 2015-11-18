package metal

import spire.algebra._

/** (Possibly null) pointer on a container element. Uses a path-dependent tag type
  * to link the pointer to its container (Pointable#Tag).
  * The type `Cap` represents the capabilities of the container.
  */
final class Ptr[T <: Pointable#Tag, +C](val raw: Long) extends AnyVal {

  override def toString = s"Ptr($raw)"

  @inline final def isNull = raw == -1L
  
  @inline final def nonNull = raw != -1L

  /* Method for name-based extractor. */
  @inline final def isEmpty = isNull

  /* Method for name-based extractor. */
  @inline final def get: VPtr[T, C] = new VPtr[T, C](raw)

}

object Ptr {

  /** Creates a pointer for the given container, with the given raw value. */

  @inline final def apply(c: Pointable, raw: Long): Ptr[c.Tag, c.Cap] = new Ptr[c.Tag, c.Cap](raw)

  /** Creates a pointer using the given raw value, according to the given container tag and the given capabilities. */
  @inline final def apply[T <: Pointable#Tag, C](raw: Long): Ptr[T, C] = new Ptr[T, C](raw)

  /** Creates a null pointer for the given container. */
  @inline final def `null`(c: Pointable): Ptr[c.Tag, c.Cap] = new Ptr[c.Tag, c.Cap](-1L)

  /** Creates a null pointer for the given container tag and the given capabilities. */
  @inline final def `null`[T <: Pointable#Tag, C]: Ptr[T, C] = new Ptr[T, C](-1L)

  // implicit conversion from valid pointer
  implicit def ptr[T <: Pointable#Tag, C](vPtr: VPtr[T, C]): Ptr[T, C] = new Ptr[T, C](vPtr.raw)

  // implicit conversions for pointers to containers with differing capabilities

  implicit def elements1[T <: Pointable#Tag, E1](ptr: Ptr[T, Elements1[E1]]): Elements1Ptr[T, E1] = new Elements1Ptr[T, E1](ptr.raw)

  implicit def keys[T <: Pointable#Tag, K](ptr: Ptr[T, Keys[K]]): KeysPtr[T, K] = new KeysPtr[T, K](ptr.raw)

  implicit def nextable[T <: Pointable#Tag, C <: Nextable](ptr: Ptr[T, C]): NextablePtr[T, C] = new NextablePtr[T, C](ptr.raw)

  implicit def values[T <: Pointable#Tag, V](ptr: Ptr[T, Values[V]]): ValuesPtr[T, V] = new ValuesPtr[T, V](ptr.raw)

  implicit def values1[T <: Pointable#Tag, V1](ptr: Ptr[T, Values1[V1]]): Values1Ptr[T, V1] = new Values1Ptr[T, V1](ptr.raw)

  implicit def values2[T <: Pointable#Tag, V2](ptr: Ptr[T, Values2[V2]]): Values2Ptr[T, V2] = new Values2Ptr[T, V2](ptr.raw)

}

final class Elements1Ptr[T <: Pointable#Tag, E1](val raw: Long) extends AnyVal {

  final def elementOrElse(orElse: E1): E1 = macro macros.PtrOps.element1OrElse[T, E1]

}

final class KeysPtr[T <: Pointable#Tag, K](val raw: Long) extends AnyVal {

  final def keyOrElse(orElse: K): K = macro macros.PtrOps.keyOrElse[T, K]

}

final class NextablePtr[T <: Pointable#Tag, +C <: Nextable](val raw: Long) extends AnyVal {

  def foreach(body: VPtr[T, C] => Unit): Unit = macro macros.PtrLoops.foreach[T, C]

  def count(body: VPtr[T, C] => Boolean): Int = macro macros.PtrLoops.count[T, C]

  def exists(body: VPtr[T, C] => Boolean): Boolean = macro macros.PtrLoops.exists[T, C]

  def forall(body: VPtr[T, C] => Boolean): Boolean = macro macros.PtrLoops.forall[T, C]

  def foldLeft[A](z: A)(body: (A, VPtr[T, C]) => A): A = macro macros.PtrLoops.foldLeft[T, C, A]

  def /:[A](z: A)(body: (A, VPtr[T, C]) => A): A = macro macros.PtrLoops.foldLeft[T, C, A]

  def minBy[A](body: VPtr[T, C] => A)(implicit orderA: Order[A]): Ptr[T, C] = macro macros.PtrLoops.minBy[T, C, A]

  def maxBy[A](body: VPtr[T, C] => A)(implicit orderA: Order[A]): Ptr[T, C] = macro macros.PtrLoops.maxBy[T, C, A]

  def sumBy[A](body: VPtr[T, C] => A)(implicit am: AdditiveMonoid[A]): A = macro macros.PtrLoops.sumBy[T, C, A]

  def productBy[A](body: VPtr[T, C] => A)(implicit mm: MultiplicativeMonoid[A]): A = macro macros.PtrLoops.productBy[T, C, A]


}

final class ValuesPtr[T <: Pointable#Tag, V](val raw: Long) extends AnyVal {

  final def valueOrElse(orElse: V): V = macro macros.PtrOps.valueOrElse[T, V]

}

final class Values1Ptr[T <: Pointable#Tag, V1](val raw: Long) extends AnyVal {

  final def valueOrElse(orElse: V1): V1 = macro macros.PtrOps.value1OrElse[T, V1]

}

final class Values2Ptr[T <: Pointable#Tag, V2](val raw: Long) extends AnyVal {

  final def valueOrElse(orElse: V2): V2 = macro macros.PtrOps.value2OrElse[T, V2]

}
