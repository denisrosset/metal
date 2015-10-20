package metal

import scala.reflect.ClassTag

import spire.syntax.cfor._

import syntax._

trait MapFactory[KLB, KExtra[_], VLB] {

  type KLBEv[K] = K <:< KLB

  def empty[K:Methods:KExtra:KLBEv, V:Methods]: MMap[K, V]

  def ofSize[K:Methods:KExtra:KLBEv, V:Methods](n: Int): MMap[K, V]

  def fromMap[K:Methods:KExtra:KLBEv, V:Methods](map: scala.collection.Map[K, V]): MMap[K, V] = {
    val mmap = empty[K, V]
    val keyIt = map.keysIterator
    while (keyIt.hasNext) {
      val k: K = keyIt.next
      mmap(k) = map(k)
    }
    mmap
  }

  def fromArrays[K:Methods:KExtra:KLBEv, V:Methods](keysArray: Array[K], valuesArray: Array[V]): MMap[K, V] = {
    require(keysArray.length == valuesArray.length)
    val mmap = empty[K, V]
    cforRange(0 until keysArray.length) { i =>
      val vp = mmap.ptrAddKeyFromArray(keysArray, i)
      mmap.ptrUpdateFromArray(vp, valuesArray, i)
    }
    mmap
  }

}
