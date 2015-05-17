package net.alasc.ptrcoll
package maps

import scala.{specialized => sp}
import scala.reflect.ClassTag

import spire.syntax.cfor._

trait MutMMap2Factory[KLB, KExtra[_], VLB1, VLB2] {
  type KLBEv[K] = K <:< KLB
  def empty[@sp(Int, Long) K: ClassTag: KExtra: KLBEv, V1: ClassTag, V2: ClassTag]: MutMMap2[K, V1, V2]
  def ofSize[@sp(Int, Long) K: ClassTag: KExtra: KLBEv, V1: ClassTag, V2: ClassTag](n: Int): MutMMap2[K, V1, V2]
  def fromMap[@sp(Int, Long) K: ClassTag: KExtra: KLBEv, V1: ClassTag, V2: ClassTag](map: Map[K, (V1, V2)]): MutMMap2[K, V1, V2] = {
    val mmap = empty[K, V1, V2]
    val keyIt = map.keysIterator
    while (keyIt.hasNext) {
      val k = keyIt.next
      val (v1, v2) = map(k)
      mmap.update(k, v1, v2)
    }
    mmap
  }
  def fromArrays[@sp(Int, Long) K: ClassTag: KExtra: KLBEv, V1: ClassTag, V2: ClassTag](keysArray: Array[K], values1Array: Array[V1], values2Array: Array[V2]): MutMMap2[K, V1, V2] = {
    val mmap = empty[K, V1, V2]
    cforRange(0 until keysArray.length) { i =>
      mmap.update(keysArray(i), values1Array(i), values2Array(i))
    }
    mmap
  }
}
