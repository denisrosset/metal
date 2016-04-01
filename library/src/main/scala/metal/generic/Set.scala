package metal
package generic

import spire.util.Opt

abstract class Set[K] extends Defaults with NElements1[K] with Enumerable with Searchable[K] { lhs =>

  implicit def K: MetalTag[K]

  type Generic = generic.Set[K]
  type Mutable <: mutable.Set[K]
  type Immutable <: immutable.Set[K]

  def stringPrefix = "Set"

  final def ptrCastT(any: Any): Opt[generic.Set[K]] = any match {
    case rhs: generic.Set[K] if lhs.K == rhs.K => Opt(rhs)
    case _ => Opt.empty[generic.Set[K]]
  }

  private[metal] def keyArray(ptr: VPtr[lhs.type]): Array[K]
  private[metal] def keyIndex(ptr: VPtr[lhs.type]): Int

  def ptrHash(ptr: VPtr[lhs.type]): Int =
    K.hashElement(keyArray(ptr), keyIndex(ptr))

  def ptrToString(ptr: VPtr[lhs.type]): String = K.toStringElement(keyArray(ptr), keyIndex(ptr))

  def ptrEquals(thisPtr: VPtr[lhs.type], that: generic.Set[K]): Boolean =
    that.ptrFindFromArray(keyArray(thisPtr), keyIndex(thisPtr)).nonNull

}
