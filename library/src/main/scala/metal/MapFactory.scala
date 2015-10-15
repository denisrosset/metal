package metal

import scala.reflect.ClassTag

import spire.syntax.cfor._

import syntax._


trait MapFactory[KLB, KExtra[_], VLB] {

  type KLBEv[K] = K <:< KLB

  def empty[K:ClassTag:KExtra:KLBEv, V:ClassTag]: Map[K, V]

  def ofSize[K:ClassTag:KExtra:KLBEv, V:ClassTag](n: Int): Map[K, V]

  def fromMap[K:ClassTag:KExtra:KLBEv, V:ClassTag](map: scala.collection.Map[K, V]): Map[K, V] = {
    val mmap = empty[K, V]
    val keyIt = map.keysIterator
    while (keyIt.hasNext) {
      val k: K = keyIt.next
      mmap(k) = map(k)
    }
    mmap
  }

  def fromArrays[K:ClassTag:KExtra:KLBEv, V:ClassTag](keysArray: Array[K], valuesArray: Array[V]): Map[K, V] = {
    val mmap = empty[K, V]
    cforRange(0 until keysArray.length) { i =>
      mmap(keysArray(i)) = valuesArray(i)
    }
    mmap
  }

}
