package metal

import spire.util.Opt

trait FSet[K] extends FColl with Elements1[K] with Enumerable with Searchable[K] with JavaMethods[FSet[K]] { lhs =>

  implicit def K: Methods[K]

  type Cap <: Nextable with Keys[K] with Elements1[K]

  type IType <: ISet[K]
  type MType <: MSet[K]

  def mutableCopy(): MSet[K] with MType

  override def stringPrefix = "FSet"

  final def ptrCastT(any: Any): Opt[FSet[K]] = any match {
    case rhs: FSet[K] if lhs.K == rhs.K => Opt(rhs)
    case _ => Opt.empty[FSet[K]]
  }

  def keyArray(ptr: MyVPtr): Array[K]
  def keyIndex(ptr: MyVPtr): Int

  def ptrHash(ptr: MyVPtr): Int =
    K.hashElement(keyArray(ptr), keyIndex(ptr))

  def ptrToString(ptr: MyVPtr): String = K.toStringElement(keyArray(ptr), keyIndex(ptr))

  def ptrEquals(thisPtr: MyVPtr, that: FSet[K]): Boolean =
    that.ptrFindFromArray(keyArray(thisPtr), keyIndex(thisPtr)).nonNull

}

trait ISet[K] extends IColl with FSet[K]

trait MSet[K] extends MColl with FSet[K] with Removable with AddKeys[K] {

  type Cap <: Nextable with Removable with Keys[K] with Elements1[K]

  def result(): ISet[K] with IType

}
