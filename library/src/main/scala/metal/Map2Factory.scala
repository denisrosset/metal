package metal

import scala.reflect.ClassTag

import spire.syntax.cfor._

import syntax._

trait Map2Factory[KLB, KExtra[_], VLB1, VLB2] {

  type KLBEv[K] = K <:< KLB

  def empty[K: ClassTag: KExtra: KLBEv, V1: ClassTag, V2: ClassTag]: Map2[K, V1, V2]

  def ofSize[K: ClassTag: KExtra: KLBEv, V1: ClassTag, V2: ClassTag](n: Int): Map2[K, V1, V2]

  def fromMap[K: ClassTag: KExtra: KLBEv, V1: ClassTag, V2: ClassTag](map: scala.collection.Map[K, (V1, V2)]): Map2[K, V1, V2] = {
    val mmap = empty[K, V1, V2]
    val keyIt = map.keysIterator
    while (keyIt.hasNext) {
      val k = keyIt.next
      val (v1, v2) = map(k)
      update2Ops(mmap).update(k, (v1, v2))
    }
    mmap
  }

  def fromArrays[K: ClassTag: KExtra: KLBEv, V1: ClassTag, V2: ClassTag](keysArray: Array[K], values1Array: Array[V1], values2Array: Array[V2]): Map2[K, V1, V2] = {
    val mmap = empty[K, V1, V2]
    cforRange(0 until keysArray.length) { i =>
      mmap.update(keysArray(i), (values1Array(i), values2Array(i)))
    }
    mmap
  }

}
