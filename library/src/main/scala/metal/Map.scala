package metal

import scala.reflect.ClassTag
import scala.annotation.tailrec

import spire.algebra.Order
import spire.util.Opt

trait FMap[K, V] extends FColl with ShapeKV with Searchable[K] with Countable with Values[V] with JavaMethods[FMap[K, V]] { lhs =>

  implicit def K: Methods[K]
  implicit def V: Methods[V]

  type IType <: IMap[K, V]
  type MType <: MMap[K, V]

  override def stringPrefix = "Map"

  final def ptrCastT(any: Any): Opt[FMap[K, V]] = any match {
    case rhs: FMap[K, V] if lhs.K == rhs.K && lhs.V == rhs.V => Opt(rhs)
    case _ => Opt.empty[FMap[K, V]]
  }

  def keyArray(ptr: VPtr[Tag]): Array[K]
  def keyIndex(ptr: VPtr[Tag]): Int

  def valueArray(ptr: VPtr[Tag]): Array[V]
  def valueIndex(ptr: VPtr[Tag]): Int

  def ptrHash(ptr: VPtr[Tag]): Int = {
    val kh = K.hashElement(keyArray(ptr), keyIndex(ptr))
    val vh = V.hashElement(valueArray(ptr), valueIndex(ptr))
    kh ^ (vh * 41)
  }

  def ptrToString(ptr: VPtr[Tag]): String =
    K.toStringElement(keyArray(ptr), keyIndex(ptr)) + " -> " + V.toStringElement(valueArray(ptr), valueIndex(ptr))

  final def ptrEquals(thisPtr: VPtr[Tag], that: FMap[K, V]): Boolean =
    that.ptrFindFromArray(keyArray(thisPtr), keyIndex(thisPtr)) match {
      case VPtr(thatPtr) =>
        val thisA = valueArray(thisPtr)
        val thisI = valueIndex(thisPtr)
        val thatA = that.valueArray(thatPtr)
        val thatI = that.valueIndex(thatPtr)
        V.equalsElement(thisA, thisI, thatA, thatI)
    }

}

trait IMap[K, V] extends IColl with FMap[K, V] {

}

trait MMap[K, V] extends MColl with FMap[K, V] with AddKeys[K] with Removable[K] with Updatable[V] {

}
