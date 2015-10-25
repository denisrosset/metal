package metal

import spire.algebra.Order

trait FSortedSet[K] extends FSet[K] {

  implicit def order: Order[K]

  type IType <: ISortedSet[K]
  type MType <: MSortedSet[K]

  def mutableCopy(): MSortedSet[K] with MType

}

trait ISortedSet[K] extends ISet[K] with FSortedSet[K] {

}


trait MSortedSet[K] extends MSet[K] with FSortedSet[K] {

  def result(): ISortedSet[K] with IType

}

object MSortedSet extends MSetFactory[Any, Order, MSortedSet] {

  import impl.SortedSetImpl

  def ofSize[K:Methods:Order:LBEv](n: Int): MSortedSet[K] = SortedSetImpl.ofAllocatedSize[K](n)

}
