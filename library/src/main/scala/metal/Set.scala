package metal

import spire.util.Opt

trait FSet[K] extends FColl with ElementsK[K] with Enumerable with Searchable[K] with JavaMethods[FSet[K]] { lhs =>

  implicit def K: Methods[K]

  type IType <: ISet[K]
  type MType <: MSet[K]

  def mutableCopy(): MSet[K] with MType

  override def stringPrefix = "FSet"

  final def ptrCastT(any: Any): Opt[FSet[K]] = any match {
    case rhs: FSet[K] if lhs.K == rhs.K => Opt(rhs)
    case _ => Opt.empty[FSet[K]]
  }

  def keyArray(ptr: VPtr[Tag]): Array[K]
  def keyIndex(ptr: VPtr[Tag]): Int

  def ptrHash(ptr: VPtr[Tag]): Int =
    K.hashElement(keyArray(ptr), keyIndex(ptr))

  def ptrToString(ptr: VPtr[Tag]): String = K.toStringElement(keyArray(ptr), keyIndex(ptr))

  def ptrEquals(thisPtr: VPtr[Tag], that: FSet[K]): Boolean =
    that.ptrFindFromArray(keyArray(thisPtr), keyIndex(thisPtr)).nonNull

}

trait ISet[K] extends IColl with FSet[K]

trait MSet[K] extends MColl with FSet[K] with Removable[K] with AddKeys[K] {

  def result(): ISet[K] with IType

}
