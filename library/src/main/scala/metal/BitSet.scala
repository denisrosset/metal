package metal

import scala.math.max
import scala.reflect.{ClassTag, classTag}
import spire.algebra.Order

import syntax._

trait FBitSet[K] extends FSortedSet[K] {

  type IType = IBitSet[K]
  type MType = MBitSet[K]

  def mutableCopy: MBitSet[K]

}

trait IBitSet[K] extends FBitSet[K] with ISortedSet[K]

trait MBitSet[K] extends FBitSet[K] with MSortedSet[K] {

  def words: Array[Long]

}

object MBitSet extends MSetFactory[Int, Dummy, MBitSet] {

  import impl.BitSetImpl

  @inline final def startSize = 2

  private[metal] def ofAllocatedWordSize[K:Methods:Dummy:LBEv](nWords: Int): MBitSet[K] = new BitSetImpl(
    words = new Array[Long](nWords),
    wordSize = 0).asInstanceOf[MBitSet[K]]

  def ofSize[K:Methods:Dummy:LBEv](n: Int): MBitSet[K] =
    ofAllocatedWordSize[K](scala.math.max(startSize, (n + 7) / 8))

}
