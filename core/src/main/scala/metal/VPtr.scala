package metal

/** Non-null pointer on a container element.
  * 
  * This class is a value class only in Scala 2.11, due to bugs in the generation
  * of bridge methods when overloading methods that take/return a value class. 
  */
final class VPtr[C <: Pointable with Singleton](val raw: Long) extends PtrVersions.Base {

  override def toString = s"VPtr($raw)"
  @inline final def isNull: Boolean = false
  @inline final def nonNull: Boolean = true

}

object VPtr {

  @inline final def apply(c: Pointable, raw: Long): VPtr[c.type] = new VPtr[c.type](raw)
  @inline final def apply[C <: Pointable with Singleton](raw: Long): VPtr[C] = new VPtr[C](raw)

  // implicit conversions for valid pointers to containers with differing capabilities

  implicit def elements1[C <: Elements1[E1] with Singleton, E1](vPtr: VPtr[C with Elements1[E1]]): Elements1VPtr[C, E1] = new Elements1VPtr[C, E1](vPtr.raw)

  implicit def elements2[C <: Elements2[E2] with Singleton, E2](vPtr: VPtr[C with Elements2[E2]]): Elements2VPtr[C, E2] = new Elements2VPtr[C, E2](vPtr.raw)

  implicit def elements3[C <: Elements3[E3] with Singleton, E3](vPtr: VPtr[C with Elements3[E3]]): Elements3VPtr[C, E3] = new Elements3VPtr[C, E3](vPtr.raw)

  implicit def keys[C <: Keys[K] with Singleton, K](vPtr: VPtr[C with Keys[K]]): KeysVPtr[C, K] = new KeysVPtr[C, K](vPtr.raw)

  implicit def nextable[C <: Nextable with Singleton](vPtr: VPtr[C]): NextableVPtr[C] = new NextableVPtr[C](vPtr.raw)

  implicit def removable[C <: Removable with Singleton](vPtr: VPtr[C]): RemovableVPtr[C] = new RemovableVPtr[C](vPtr.raw)

  implicit def updatable[C <: Updatable[V] with Singleton, V](vPtr: VPtr[C with Updatable[V]]): UpdatableVPtr[C, V] = new UpdatableVPtr[C, V](vPtr.raw)

  implicit def updatable1[C <: Updatable1[V1] with Singleton, V1](vPtr: VPtr[C with Updatable1[V1]]): Updatable1VPtr[C, V1] = new Updatable1VPtr[C, V1](vPtr.raw)

  implicit def updatable2[C <: Updatable2[V2] with Singleton, V2](vPtr: VPtr[C with Updatable2[V2]]): Updatable2VPtr[C, V2] = new Updatable2VPtr[C, V2](vPtr.raw)

  implicit def values[C <: Values[V] with Singleton, V](vPtr: VPtr[C with Values[V]]): ValuesVPtr[C, V] = new ValuesVPtr[C, V](vPtr.raw)

  implicit def values1[C <: Values1[V1] with Singleton, V1](vPtr: VPtr[C with Values1[V1]]): Values1VPtr[C, V1] = new Values1VPtr[C, V1](vPtr.raw)

  implicit def values2[C <: Values2[V2] with Singleton, V2](vPtr: VPtr[C with Values2[V2]]): Values2VPtr[C, V2] = new Values2VPtr[C, V2](vPtr.raw)
   
}

// capability-based valid pointer types

final class Elements1VPtr[C <: Elements1[E1] with Singleton, E1](val raw: Long) extends AnyVal {

  final def element: E1 = macro macros.VPtrOps.element1[C, E1]

  final def element1: E1 = macro macros.VPtrOps.element1[C, E1]

}

final class Elements2VPtr[C <: Elements2[E2] with Singleton, E2](val raw: Long) extends AnyVal {

  final def element2: E2 = macro macros.VPtrOps.element2[C, E2]

}

final class Elements3VPtr[C <: Elements3[E3] with Singleton, E3](val raw: Long) extends AnyVal {

  final def element3: E3 = macro macros.VPtrOps.element3[C, E3]

}

final class KeysVPtr[C <: Keys[K] with Singleton, K](val raw: Long) extends AnyVal {

  final def key: K = macro macros.VPtrOps.key[C, K]

}

final class NextableVPtr[C <: Nextable with Singleton](val raw: Long) extends AnyVal {

  final def next: Ptr[C] = macro macros.VPtrOps.next[C]

}

final class RemovableVPtr[C <: Removable with Singleton](val raw: Long) extends AnyVal {

  final def remove(): Unit = macro macros.VPtrOps.remove[C]
  final def removeAndAdvance(): Ptr[C] = macro macros.VPtrOps.removeAndAdvance[C]

}

final class UpdatableVPtr[C <: Updatable[V] with Singleton, V](val raw: Long) extends AnyVal {

  final def value_=(newValue: V): Unit = macro macros.VPtrOps.update[C, V]

}

final class Updatable1VPtr[C <: Updatable1[V1] with Singleton, V1](val raw: Long) extends AnyVal {

  final def value1_=(newValue1: V1): Unit = macro macros.VPtrOps.update1[C, V1]

}

final class Updatable2VPtr[C <: Updatable2[V2] with Singleton, V2](val raw: Long) extends AnyVal {

  final def value2_=(newValue2: V2): Unit = macro macros.VPtrOps.update2[C, V2]

}

final class ValuesVPtr[C <: Values[V] with Singleton, V](val raw: Long) extends AnyVal {

  final def value: V = macro macros.VPtrOps.value[C, V]

}

final class Values1VPtr[C <: Values1[V1] with Singleton, V1](val raw: Long) extends AnyVal {

  final def value1: V1 = macro macros.VPtrOps.value1[C, V1]

}

final class Values2VPtr[C <: Values2[V2] with Singleton, V2](val raw: Long) extends AnyVal {

  final def value2: V2 = macro macros.VPtrOps.value2[C, V2]

}
