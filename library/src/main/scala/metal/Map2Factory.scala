package metal

import scala.reflect.ClassTag

import spire.syntax.cfor._

import syntax._

trait MMap2Factory[KLB, KExtra[_], VLB1, VLB2, MP2[_, _, _] <: MMap2[_, _, _]] {

  type KLBEv[K] = K <:< KLB

  def empty[K:Methods:KExtra:KLBEv, V1:Methods, V2:Methods]: MP2[K, V1, V2] = ofSize[K, V1, V2](0)

  def apply[K:Methods:KExtra:KLBEv, V1:Methods, V2:Methods](kv1v2s: (K, (V1, V2))*): MP2[K, V1, V2] = {
    val mmap2 = empty[K, V1, V2]
    val it = kv1v2s.iterator
    while (it.hasNext) {
      val pair = it.next
      val vp = mmap2.ptrAddKey(pair._1)
      mmap2.ptrUpdate1(vp, pair._2._1)
      mmap2.ptrUpdate2(vp, pair._2._2)
    }
    mmap2
  }

  def ofSize[K:Methods:KExtra:KLBEv, V1:Methods, V2:Methods](n: Int): MP2[K, V1, V2]

  def fromMap[K:Methods:KExtra:KLBEv, V1:Methods, V2:Methods](map: scala.collection.Map[K, (V1, V2)]): MP2[K, V1, V2] = {
    val mmap = empty[K, V1, V2]
    val keyIt = map.keysIterator
    while (keyIt.hasNext) {
      val k = keyIt.next
      val (v1, v2) = map(k)
      val vp = mmap.ptrAddKey(k)
      mmap.ptrUpdate1(vp, v1)
      mmap.ptrUpdate2(vp, v2)
    }
    mmap
  }

  def fromArrays[K:Methods: KExtra: KLBEv, V1:Methods, V2:Methods](keysArray: Array[K], values1Array: Array[V1], values2Array: Array[V2]): MP2[K, V1, V2] = {
    val mmap = empty[K, V1, V2]
    cforRange(0 until keysArray.length) { i =>
      val vp = mmap.ptrAddKeyFromArray(keysArray, i)
      mmap.ptrUpdate1FromArray(vp, values1Array, i)
      mmap.ptrUpdate2FromArray(vp, values2Array, i)
    }
    mmap
  }

}
