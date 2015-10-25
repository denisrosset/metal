package metal

import scala.reflect.ClassTag

import spire.syntax.cfor._

import syntax._

trait MapFactory[KLB, KExtra[_], VLB, MP[_, _] <: MMap[_, _]] {

  type KLBEv[K] = K <:< KLB

  def empty[K:Methods:KExtra:KLBEv, V:Methods]: MP[K, V] = ofSize[K, V](0)

  def ofSize[K:Methods:KExtra:KLBEv, V:Methods](n: Int): MP[K, V]

  def fromMap[K:Methods:KExtra:KLBEv, V:Methods](map: scala.collection.Map[K, V]): MP[K, V] = {
    val mmap = empty[K, V]
    val keyIt = map.keysIterator
    while (keyIt.hasNext) {
      val k: K = keyIt.next
      val vp = mmap.ptrAddKey(k)
      mmap.ptrUpdate(vp, map(k))
    }
    mmap
  }

  def fromArrays[K:Methods:KExtra:KLBEv, V:Methods](keysArray: Array[K], valuesArray: Array[V]): MP[K, V] = {
    require(keysArray.length == valuesArray.length)
    val mmap = empty[K, V]
    cforRange(0 until keysArray.length) { i =>
      val vp = mmap.ptrAddKeyFromArray(keysArray, i)
      mmap.ptrUpdateFromArray(vp, valuesArray, i)
    }
    mmap
  }

}
