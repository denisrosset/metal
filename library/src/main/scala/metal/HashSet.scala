package metal

import scala.annotation.{switch, tailrec}
import scala.reflect.ClassTag

import spire.algebra.Order
import spire.util.Opt

trait FHashSet[K] extends FSet[K] {

  type IType = IHashSet[K]
  type MType = MHashSet[K]

  def mutableCopy: MHashSet[K]

}

trait IHashSet[K] extends FHashSet[K] with ISet[K]

trait MHashSet[K] extends FHashSet[K] with MSet[K] {

  def len: Int
  def used: Int
  def mask: Int
  def limit: Int
  def keys: Array[K]
  def buckets: Array[Byte]

  def result(): IHashSet[K]

}

object MHashSet extends MSetFactory[Any, Dummy, MHashSet] {

  import impl.HashSetImpl

  def ofSize[K:Methods:Dummy:LBEv](n: Int): MHashSet[K] = HashSetImpl.ofAllocatedSize(n / 2 * 3)

}
