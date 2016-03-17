package metal

import spire.util.Opt

abstract class Set[K] extends Defaults with NElements1[K] with Enumerable with Searchable[K] { lhs =>

  implicit def K: Methods[K]

  type Generic = metal.Set[K]
  type Mutable <: metal.mutable.Set[K]
  type Immutable <: metal.immutable.Set[K]

  def stringPrefix = "Set"

  final def ptrCastT(any: Any): Opt[metal.Set[K]] = any match {
    case rhs: metal.Set[K] if lhs.K == rhs.K => Opt(rhs)
    case _ => Opt.empty[metal.Set[K]]
  }

  private[metal] def keyArray(ptr: VPtr[lhs.type]): Array[K]
  private[metal] def keyIndex(ptr: VPtr[lhs.type]): Int

  def ptrHash(ptr: VPtr[lhs.type]): Int =
    K.hashElement(keyArray(ptr), keyIndex(ptr))

  def ptrToString(ptr: VPtr[lhs.type]): String = K.toStringElement(keyArray(ptr), keyIndex(ptr))

  def ptrEquals(thisPtr: VPtr[lhs.type], that: metal.Set[K]): Boolean =
    that.ptrFindFromArray(keyArray(thisPtr), keyIndex(thisPtr)).nonNull

}
