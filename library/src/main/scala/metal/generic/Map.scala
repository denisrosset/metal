package metal
package generic

import spire.util.Opt

abstract class Map[K, V] extends Defaults with Enumerable with Searchable[K] with Values[V] with NElements2[K, V] { lhs =>

  implicit def K: MetalTag[K]
  implicit def V: MetalTag[V]

  type Generic = generic.Map[K, V]
  type Mutable <: mutable.Map[K, V]
  type Immutable <: immutable.Map[K, V]

  override def stringPrefix = "Map"

  final def ptrCastT(any: Any): Opt[generic.Map[K, V]] = any match {
    case rhs: generic.Map[K, V] if lhs.K == rhs.K && lhs.V == rhs.V => Opt(rhs)
    case _ => Opt.empty[generic.Map[K, V]]
  }

  private[metal] def keyArray(ptr: VPtr[lhs.type]): Array[K]
  private[metal] def keyIndex(ptr: VPtr[lhs.type]): Int
  private[metal] def valueArray(ptr: VPtr[lhs.type]): Array[V]
  private[metal] def valueIndex(ptr: VPtr[lhs.type]): Int

  def ptrHash(ptr: VPtr[lhs.type]): Int = {
    val kh = K.hashElement(keyArray(ptr), keyIndex(ptr))
    val vh = V.hashElement(valueArray(ptr), valueIndex(ptr))
    kh ^ (vh * 41)
  }

  def ptrToString(ptr: VPtr[lhs.type]): String =
    K.toStringElement(keyArray(ptr), keyIndex(ptr)) + " -> " + V.toStringElement(valueArray(ptr), valueIndex(ptr))

  final def ptrEquals(thisPtr: VPtr[lhs.type], that: Map[K, V]): Boolean =
    that.ptrFindFromArray(keyArray(thisPtr), keyIndex(thisPtr)) match {
      case IsVPtr(thatPtr) =>
        val thisA = valueArray(thisPtr)
        val thisI = valueIndex(thisPtr)
        val thatA = that.valueArray(thatPtr)
        val thatI = that.valueIndex(thatPtr)
        V.equalsElement(thisA, thisI, thatA, thatI)
      case _ => false
    }

}
