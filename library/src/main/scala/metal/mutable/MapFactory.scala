package metal
package mutable

import spire.syntax.cfor._
import generic.Methods

trait MapFactory extends generic.MapFactory {

  type M[K, V] <: mutable.Map[K, V]

  def empty[K:Methods:KExtra, V:Methods:VExtra]: M[K, V] = reservedSize[K, V](0)

  def reservedSize[K:Methods:KExtra, V:Methods:VExtra](n: Int): M[K, V]

  def apply[K:Methods:KExtra, V:Methods:VExtra](kvPairs: (K, V)*) = fromIterable(kvPairs)

  def fromIterable[K:Methods:KExtra, V:Methods:VExtra](kvPairs: Iterable[(K, V)]) = {
    val map = empty[K, V]
    val pairIt = kvPairs.iterator
    while (pairIt.hasNext) {
      val pair = pairIt.next
      val vp = map.ptrAddKey(pair._1)
      map.ptrUpdate(vp, pair._2)
    }
    map
  }

  def fromMap[K:Methods:KExtra, V:Methods:VExtra](source: scala.collection.Map[K, V]) = {
    val map = reservedSize[K, V](source.size)
    val keyIt = source.keysIterator
    while (keyIt.hasNext) {
      val k: K = keyIt.next
      val vp = map.ptrAddKey(k)
      map.ptrUpdate(vp, source(k))
    }
    map
  }

  def fromArrays[K:Methods:KExtra, V:Methods:VExtra](keysArray: Array[K], valuesArray: Array[V]) = {
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
