package metal

import spire.algebra._

/** Non-null pointer on a container element. */
final class VPtr[T <: Pointable#Tag, +C](val raw: Long) extends AnyVal {

  override def toString = s"VPtr($raw)"
  @inline final def isNull: Boolean = false
  @inline final def nonNull: Boolean = true

}

object VPtr {

  @inline final def apply[T <: Pointable#Tag, C](raw: Long): VPtr[T, C] = new VPtr[T, C](raw)

  // implicit conversions for valid pointers to containers with differing capabilities

  implicit def elements[T <: Pointable#Tag, E](vPtr: VPtr[T, Elements[E]]): ElementsVPtr[T, E] = new ElementsVPtr[T, E](vPtr.raw)

  implicit def keys[T <: Pointable#Tag, A](vPtr: VPtr[T, Keys[A]]): KeysVPtr[T, A] = new KeysVPtr[T, A](vPtr.raw)

  implicit def nextable[T <: Pointable#Tag, C <: Nextable](vPtr: VPtr[T, C]): NextableVPtr[T, C] = new NextableVPtr[T, C](vPtr.raw)

  implicit def removable[T <: Pointable#Tag, C <: Removable](vPtr: VPtr[T, C]): RemovableVPtr[T, C] = new RemovableVPtr[T, C](vPtr.raw)

  implicit def updatable[T <: Pointable#Tag, V](vPtr: VPtr[T, Updatable[V]]): UpdatableVPtr[T, V] = new UpdatableVPtr[T, V](vPtr.raw)

  implicit def updatable1[T <: Pointable#Tag, V1](vPtr: VPtr[T, Updatable1[V1]]): Updatable1VPtr[T, V1] = new Updatable1VPtr[T, V1](vPtr.raw)

  implicit def updatable2[T <: Pointable#Tag, V2](vPtr: VPtr[T, Updatable2[V2]]): Updatable2VPtr[T, V2] = new Updatable2VPtr[T, V2](vPtr.raw)

  implicit def values[T <: Pointable#Tag, V](vPtr: VPtr[T, Values[V]]): ValuesVPtr[T, V] = new ValuesVPtr[T, V](vPtr.raw)

  implicit def values1[T <: Pointable#Tag, V1](vPtr: VPtr[T, Values1[V1]]): Values1VPtr[T, V1] = new Values1VPtr[T, V1](vPtr.raw)

  implicit def values2[T <: Pointable#Tag, V2](vPtr: VPtr[T, Values2[V2]]): Values2VPtr[T, V2] = new Values2VPtr[T, V2](vPtr.raw)

}

object IsVPtr {

  // for name-based extraction
  @inline final def unapply[T <: Pointable#Tag, C](ptr: Ptr[T, C]): Ptr[T, C] = ptr

}

// capability-based valid pointer types

final class ElementsVPtr[T <: Pointable#Tag, E](val raw: Long) extends AnyVal {

  final def element: E = macro macros.VPtrOps.element[T, E]

}

final class KeysVPtr[T <: Pointable#Tag, A](val raw: Long) extends AnyVal {

  final def key: A = macro macros.VPtrOps.key[T, A]

}

final class NextableVPtr[T <: Pointable#Tag, +C <: Nextable](val raw: Long) extends AnyVal {

  final def next: Ptr[T, C] = macro macros.VPtrOps.next[T, C]

}

final class RemovableVPtr[T <: Pointable#Tag, +C <: Removable](val raw: Long) extends AnyVal {

  final def remove: Unit = macro macros.VPtrOps.remove[T]
  final def removeAndAdvance: Ptr[T, C] = macro macros.VPtrOps.removeAndAdvance[T, C]

}

final class UpdatableVPtr[T <: Pointable#Tag, V](val raw: Long) extends AnyVal {

  final def value_=(newValue: V): Unit = macro macros.VPtrOps.update[T, V]

}

final class Updatable1VPtr[T <: Pointable#Tag, V1](val raw: Long) extends AnyVal {

  final def value1_=(newValue1: V1): Unit = macro macros.VPtrOps.update1[T, V1]

}

final class Updatable2VPtr[T <: Pointable#Tag, V2](val raw: Long) extends AnyVal {

  final def value2_=(newValue2: V2): Unit = macro macros.VPtrOps.update2[T, V2]

}

final class ValuesVPtr[T <: Pointable#Tag, V](val raw: Long) extends AnyVal {

  final def value: V = macro macros.VPtrOps.value[T, V]

}

final class Values1VPtr[T <: Pointable#Tag, V1](val raw: Long) extends AnyVal {

  final def value1: V1 = macro macros.VPtrOps.value1[T, V1]

}

final class Values2VPtr[T <: Pointable#Tag, V2](val raw: Long) extends AnyVal {

  final def value2: V2 = macro macros.VPtrOps.value2[T, V2]

}
