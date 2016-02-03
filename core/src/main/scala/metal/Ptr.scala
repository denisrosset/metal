package metal

import spire.algebra._

/** (Possibly null) pointer on a container element. Uses a singleton type
  * to link the pointer to its container.
  * 
  * This class is a value class only in Scala 2.11, due to bugs in the generation
  * of bridge methods when overloading methods that take/return a value class. 
  */
final class Ptr[C <: Pointable with Singleton] protected[Ptr] (val raw: Long) extends PtrVersions.Base {

  override def toString = s"Ptr($raw)"

  @inline final def isNull = raw == -1L
  @inline final def nonNull = raw != -1L

  /* Method for name-based extractor. */
  @inline final def isEmpty = isNull

  /* Method for name-based extractor. */
  @inline final def get: VPtr[C] = VPtr[C](raw)

}

object Ptr {

  /** Creates a pointer for the given container, with the given raw value. */
  @inline final def apply(c: Pointable, raw: Long): Ptr[c.type] = new Ptr[c.type](raw)

  /** Creates a pointer using the given raw value for the given container singleton type. */
  @inline final def apply[C <: Pointable with Singleton](raw: Long): Ptr[C] = new Ptr[C](raw)

  /** Creates a null pointer for the given container. */
  @inline final def Null(c: Pointable): Ptr[c.type] = new Ptr[c.type](-1L)

  /** Creates a null pointer for the given container singleton type. */
  @inline final def Null[C <: Pointable with Singleton]: Ptr[C] = new Ptr[C](-1L)

  // implicit conversion from valid pointer
  implicit def ptr[C <: Pointable with Singleton](vPtr: VPtr[C]): Ptr[C] = new Ptr[C](vPtr.raw)

  // implicit conversions for pointers to containers with differing capabilities

  implicit def elements1[C <: Elements1[E1] with Singleton, E1](ptr: Ptr[C with Elements1[E1]]): Elements1Ptr[C, E1] = new Elements1Ptr[C, E1](ptr.raw)

  implicit def elements2[C <: Elements2[E2] with Singleton, E2](ptr: Ptr[C with Elements2[E2]]): Elements2Ptr[C, E2] = new Elements2Ptr[C, E2](ptr.raw)

  implicit def elements3[C <: Elements3[E3] with Singleton, E3](ptr: Ptr[C with Elements3[E3]]): Elements3Ptr[C, E3] = new Elements3Ptr[C, E3](ptr.raw)

  implicit def keys[C <: Keys[K] with Singleton, K](ptr: Ptr[C with Keys[K]]): KeysPtr[C, K] = new KeysPtr[C, K](ptr.raw)

  implicit def nextable[C <: Nextable with Singleton](ptr: Ptr[C]): NextablePtr[C] = new NextablePtr[C](ptr.raw)

  implicit def values[C <: Values[V] with Singleton, V](ptr: Ptr[C with Values[V]]): ValuesPtr[C, V] = new ValuesPtr[C, V](ptr.raw)

  implicit def values1[C <: Values1[V1] with Singleton, V1](ptr: Ptr[C with Values1[V1]]): Values1Ptr[C, V1] = new Values1Ptr[C, V1](ptr.raw)

  implicit def values2[C <: Values2[V2] with Singleton, V2](ptr: Ptr[C with Values2[V2]]): Values2Ptr[C, V2] = new Values2Ptr[C, V2](ptr.raw)

}

final class Elements1Ptr[C <: Elements1[E1] with Singleton, E1](val raw: Long) extends AnyVal {

  final def elementOrElse(orElse: E1): E1 =  macro macros.PtrOps.element1OrElse[C, E1]

  final def element1OrElse(orElse: E1): E1 =  macro macros.PtrOps.element1OrElse[C, E1]

}

final class Elements2Ptr[C <: Elements2[E2] with Singleton, E2](val raw: Long) extends AnyVal {

  final def element2OrElse(orElse: E2): E2 =  macro macros.PtrOps.element2OrElse[C, E2]

}

final class Elements3Ptr[C <: Elements3[E3] with Singleton, E3](val raw: Long) extends AnyVal {

  final def element3OrElse(orElse: E3): E3 =  macro macros.PtrOps.element3OrElse[C, E3]

}

final class KeysPtr[C <: Keys[K] with Singleton, K](val raw: Long) extends AnyVal {

  final def keyOrElse(orElse: K): K =  macro macros.PtrOps.keyOrElse[C, K]

}

final class NextablePtr[C <: Nextable with Singleton](val raw: Long) extends AnyVal {

  def foreach(body: VPtr[C] => Unit): Unit = macro macros.PtrLoops.foreach[C]

  def count(body: VPtr[C] => Boolean): Int = macro macros.PtrLoops.count[C]

  def exists(body: VPtr[C] => Boolean): Boolean =  macro macros.PtrLoops.exists[C]

  def forall(body: VPtr[C] => Boolean): Boolean =  macro macros.PtrLoops.forall[C]

  def foldLeft[A](z: A)(body: (A, VPtr[C]) => A): A =  macro macros.PtrLoops.foldLeft[C, A]

  def /:[A](z: A)(body: (A, VPtr[C]) => A): A =  macro macros.PtrLoops.foldLeft[C, A]

  def minBy[A](body: VPtr[C] => A)(implicit orderA: Order[A]): Ptr[C] =  macro macros.PtrLoops.minBy[C, A]

  def maxBy[A](body: VPtr[C] => A)(implicit orderA: Order[A]): Ptr[C] =  macro macros.PtrLoops.maxBy[C, A]

  def sumBy[A](body: VPtr[C] => A)(implicit am: AdditiveMonoid[A]): A =  macro macros.PtrLoops.sumBy[C, A]

  def productBy[A](body: VPtr[C] => A)(implicit mm: MultiplicativeMonoid[A]): A = macro macros.PtrLoops.productBy[C, A]


}

final class ValuesPtr[T <: Values[V] with Singleton, V](val raw: Long) extends AnyVal {

  final def valueOrElse(orElse: V): V =  macro macros.PtrOps.valueOrElse[T, V]

}

final class Values1Ptr[T <: Values1[V1] with Singleton, V1](val raw: Long) extends AnyVal {

  final def value1OrElse(orElse: V1): V1 =  macro macros.PtrOps.value1OrElse[T, V1]

}

final class Values2Ptr[T <: Values2[V2] with Singleton, V2](val raw: Long) extends AnyVal {

  final def value2OrElse(orElse: V2): V2 =  macro macros.PtrOps.value2OrElse[T, V2]

}
