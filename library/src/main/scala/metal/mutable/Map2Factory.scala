package metal.mutable

import spire.syntax.cfor._
import metal.Methods

trait Map2Factory extends metal.Map2Factory {

  type M[K, V1, V2] <: metal.mutable.Map2[K, V1, V2]

  def reservedSize[K:Methods:KExtra, V1:Methods:V1Extra, V2:Methods:V2Extra](n: Int): M[K, V1, V2]

  def empty[K:Methods:KExtra, V1:Methods:V1Extra, V2:Methods:V2Extra]: M[K, V1, V2] = reservedSize[K, V1, V2](0)

  def apply[K:Methods:KExtra, V1:Methods:V1Extra, V2:Methods:V2Extra](kv1v2s: (K, (V1, V2))*) =
    fromIterable(kv1v2s)

  def fromIterable[K:Methods:KExtra, V1:Methods:V1Extra, V2:Methods:V2Extra](kv1v2s: Iterable[(K, (V1, V2))]) = {
    val map2 = empty[K, V1, V2]
    val it = kv1v2s.iterator
    while (it.hasNext) {
      val pair = it.next
      val vp = map2.ptrAddKey(pair._1)
      map2.ptrUpdate1(vp, pair._2._1)
      map2.ptrUpdate2(vp, pair._2._2)
    }
    map2
  }

  def fromMap[K:Methods:KExtra, V1:Methods:V1Extra, V2:Methods:V2Extra](map: scala.collection.Map[K, (V1, V2)]) = {
    val map2 = empty[K, V1, V2]
    val keyIt = map.keysIterator
    while (keyIt.hasNext) {
      val k = keyIt.next
      val (v1, v2) = map(k)
      val vp = map2.ptrAddKey(k)
      map2.ptrUpdate1(vp, v1)
      map2.ptrUpdate2(vp, v2)
    }
    map2
  }

  def fromArrays[K:Methods:KExtra, V1:Methods:V1Extra, V2:Methods:V2Extra](keysArray: Array[K], values1Array: Array[V1], values2Array: Array[V2]) = {
    val map2 = empty[K, V1, V2]
    cforRange(0 until keysArray.length) { i =>
      val vp = map2.ptrAddKeyFromArray(keysArray, i)
      map2.ptrUpdate1FromArray(vp, values1Array, i)
      map2.ptrUpdate2FromArray(vp, values2Array, i)
    }
    map2
  }

}
