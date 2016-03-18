package metal

import scala.annotation.tailrec

import spire.algebra.Order
import spire.util.Opt

abstract class Map2[K, V1, V2] extends Defaults with Enumerable with Searchable[K] with Values1[V1] with Values2[V2] with NElements3[K, V1, V2] { lhs =>

  implicit def K: Methods[K]
  implicit def V1: Methods[V1]
  implicit def V2: Methods[V2]

  type Generic = metal.Map2[K, V1, V2]
  type Mutable <: metal.mutable.Map2[K, V1, V2]
  type Immutable <: metal.immutable.Map2[K, V1, V2]

  override def stringPrefix = "Map2"

  final def ptrCastT(any: Any): Opt[metal.Map2[K, V1, V2]] = any match {
    case rhs: metal.Map2[K, V1, V2] if lhs.K == rhs.K && lhs.V1 == rhs.V1 && lhs.V2 == rhs.V2 => Opt(rhs)
    case _ => Opt.empty[metal.Map2[K, V1, V2]]
  }

  private[metal] def keyArray(ptr: VPtr[lhs.type]): Array[K]
  private[metal] def keyIndex(ptr: VPtr[lhs.type]): Int
  private[metal] def value1Array(ptr: VPtr[lhs.type]): Array[V1]
  private[metal] def value1Index(ptr: VPtr[lhs.type]): Int
  private[metal] def value2Array(ptr: VPtr[lhs.type]): Array[V2]
  private[metal] def value2Index(ptr: VPtr[lhs.type]): Int

  def ptrHash(ptr: VPtr[this.type]): Int = {
    val kh = K.hashElement(keyArray(ptr), keyIndex(ptr))
    val v1h = V1.hashElement(value1Array(ptr), value1Index(ptr))
    val v2h = V2.hashElement(value2Array(ptr), value2Index(ptr))
    kh ^ (v1h * 41) ^ (v2h * 41 * 41)
  }

  def ptrToString(ptr: VPtr[this.type]): String = {
    val ks = K.toStringElement(keyArray(ptr), keyIndex(ptr))
    val v1s = V1.toStringElement(value1Array(ptr), value1Index(ptr))
    val v2s = V2.toStringElement(value2Array(ptr), value2Index(ptr))
    s"$ks -> ($v1s, $v2s)"
  }

  final def ptrEquals(thisPtr: VPtr[this.type], that: metal.Map2[K, V1, V2]): Boolean =
    that.ptrFindFromArray(keyArray(thisPtr), keyIndex(thisPtr)) match {
      case IsVPtr(thatPtr) =>
        val thisA1 = value1Array(thisPtr)
        val thisI1 = value1Index(thisPtr)
        val thisA2 = value2Array(thisPtr)
        val thisI2 = value2Index(thisPtr)
        val thatA1 = that.value1Array(thatPtr)
        val thatI1 = that.value1Index(thatPtr)
        val thatA2 = that.value2Array(thatPtr)
        val thatI2 = that.value2Index(thatPtr)
        V1.equalsElement(thisA1, thisI1, thatA1, thatI1) &&
        V2.equalsElement(thisA2, thisI2, thatA2, thatI2)
      case _ => false
    }

}
