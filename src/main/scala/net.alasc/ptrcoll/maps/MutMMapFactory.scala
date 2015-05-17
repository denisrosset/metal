package net.alasc.ptrcoll
package maps

import scala.{specialized => sp}
import scala.reflect.ClassTag

import spire.syntax.cfor._

trait MutMMapFactory[KLB, KExtra[_], VLB] {
  type KLBEv[K] = K <:< KLB
  def empty[@sp(Int, Long) K: ClassTag: KExtra: KLBEv, V: ClassTag]: MutMMap[K, V]
  def ofSize[@sp(Int, Long) K: ClassTag: KExtra: KLBEv, V: ClassTag](n: Int): MutMMap[K, V]
  def fromMap[@sp(Int, Long) K: ClassTag: KExtra: KLBEv, V: ClassTag](map: Map[K, V]): MutMMap[K, V] = {
    val mmap = empty[K, V]
    val keyIt = map.keysIterator
    while (keyIt.hasNext) {
      val k = keyIt.next
      mmap(k) = map(k)
    }
    mmap
  }
  def fromArrays[@sp(Int, Long) K: ClassTag: KExtra: KLBEv, V: ClassTag](keysArray: Array[K], valuesArray: Array[V]): MutMMap[K, V] = {
    val mmap = empty[K, V]
    cforRange(0 until keysArray.length) { i =>
      mmap(keysArray(i)) = valuesArray(i)
    }
    mmap
  }
}
