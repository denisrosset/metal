package metal

import scala.reflect.ClassTag
import scala.annotation.tailrec

import spire.algebra.Order
import spire.util.Opt

trait FMap[K, V] extends FColl with Searchable[K] with Enumerable with Values[V] with Elements2[K, V] with JavaMethods[FMap[K, V]] { lhs =>

  implicit def K: Methods[K]
  implicit def V: Methods[V]

  type Cap <: Nextable with Keys[K] with Values[V] with Elements2[K, V]

  type IType <: IMap[K, V]
  type MType <: MMap[K, V]

  def mutableCopy: MMap[K, V] with MType

  override def stringPrefix = "FMap"

  final def ptrCastT(any: Any): Opt[FMap[K, V]] = any match {
    case rhs: FMap[K, V] if lhs.K == rhs.K && lhs.V == rhs.V => Opt(rhs)
    case _ => Opt.empty[FMap[K, V]]
  }

  def keyArray(ptr: MyVPtr): Array[K]
  def keyIndex(ptr: MyVPtr): Int
  def valueArray(ptr: MyVPtr): Array[V]
  def valueIndex(ptr: MyVPtr): Int

  def ptrHash(ptr: MyVPtr): Int = {
    val kh = K.hashElement(keyArray(ptr), keyIndex(ptr))
    val vh = V.hashElement(valueArray(ptr), valueIndex(ptr))
    kh ^ (vh * 41)
  }

  def ptrToString(ptr: MyVPtr): String =
    K.toStringElement(keyArray(ptr), keyIndex(ptr)) + " -> " + V.toStringElement(valueArray(ptr), valueIndex(ptr))

  final def ptrEquals(thisPtr: MyVPtr, that: FMap[K, V]): Boolean =
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

trait IMap[K, V] extends IColl with FMap[K, V]

trait MMap[K, V] extends MColl with FMap[K, V] with AddKeys[K] with Removable with Updatable[V] {

  type Cap <: Nextable with Removable with Keys[K] with Values[V] with Updatable[V] with Elements2[K, V]

  def result(): IMap[K, V] with IType

}
