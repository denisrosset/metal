package metal
package mutable

import spire.syntax.cfor._

trait MapFactory extends generic.MapFactory {

  type M[K, V] <: mutable.Map[K, V]

  def empty[K:MetalTag:KExtra, V:MetalTag:VExtra]: M[K, V] = reservedSize[K, V](0)

  def reservedSize[K:MetalTag:KExtra, V:MetalTag:VExtra](n: Int): M[K, V]

  def apply[K:MetalTag:KExtra, V:MetalTag:VExtra](kvPairs: (K, V)*) = fromIterable(kvPairs)

  def fromIterable[K:MetalTag:KExtra, V:MetalTag:VExtra](kvPairs: Iterable[(K, V)]) = {
    val map = empty[K, V]
    val pairIt = kvPairs.iterator
    while (pairIt.hasNext) {
      val pair = pairIt.next
      val vp = map.ptrAddKey(pair._1)
      map.ptrUpdate(vp, pair._2)
    }
    map
  }

  def fromMap[K:MetalTag:KExtra, V:MetalTag:VExtra](source: scala.collection.Map[K, V]) = {
    val map = reservedSize[K, V](source.size)
    val keyIt = source.keysIterator
    while (keyIt.hasNext) {
      val k: K = keyIt.next
      val vp = map.ptrAddKey(k)
      map.ptrUpdate(vp, source(k))
    }
    map
  }

  def fromArrays[K:MetalTag:KExtra, V:MetalTag:VExtra](keysArray: Array[K], valuesArray: Array[V]) = {
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
