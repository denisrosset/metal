package metal.mutable

import spire.syntax.cfor._
import metal.{Methods}

trait MapFactory extends metal.MapFactory {

  type KExtra[_]

  type M[K, V] <: metal.mutable.Map[K, V]

  def empty[K:Methods:KExtra, V:Methods]: M[K, V] = reservedSize[K, V](0)

  def reservedSize[K:Methods:KExtra, V:Methods](n: Int): M[K, V]

  def apply[K:Methods:KExtra, V:Methods](kvPairs: (K, V)*): M[K, V] = {
    val map = empty[K, V]
    val pairIt = kvPairs.iterator
    while (pairIt.hasNext) {
      val pair = pairIt.next
      val vp = map.ptrAddKey(pair._1)
      map.ptrUpdate(vp, pair._2)
    }
    map
  }

  def fromMap[K:Methods:KExtra, V:Methods](source: scala.collection.Map[K, V]): M[K, V] = {
    val map = reservedSize[K, V](source.size)
    val keyIt = source.keysIterator
    while (keyIt.hasNext) {
      val k: K = keyIt.next
      val vp = map.ptrAddKey(k)
      map.ptrUpdate(vp, source(k))
    }
    map
  }

  def fromArrays[K:Methods:KExtra, V:Methods](keysArray: Array[K], valuesArray: Array[V]): M[K, V] = {
    val n = keysArray.length
    require(n == valuesArray.length)
    val map = reservedSize[K, V](n)
    cforRange(0 until n) { i =>
      val vp = map.ptrAddKeyFromArray(keysArray, i)
      map.ptrUpdateFromArray(vp, valuesArray, i)
    }
    map
  }

}
