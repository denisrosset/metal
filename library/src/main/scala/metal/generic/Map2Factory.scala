package metal
package generic

import scala.reflect.ClassTag

trait Map2Factory {

  type KExtra[_]

  type V1Extra[_]

  type V2Extra[_]

  type M[K, V1, V2] <: generic.Map2[K, V1, V2]

  def empty[K:ClassTag:KExtra, V1:ClassTag:V1Extra, V2:ClassTag:V2Extra]: M[K, V1, V2]

  def apply[K:ClassTag:KExtra, V1:ClassTag:V1Extra, V2:ClassTag:V2Extra](kv1v2s: (K, (V1, V2))*): M[K, V1, V2]

  def fromMap[K:ClassTag:KExtra, V1:ClassTag:V1Extra, V2:ClassTag:V2Extra](map: scala.collection.Map[K, (V1, V2)]): M[K, V1, V2]

  def fromArrays[K:ClassTag:KExtra, V1:ClassTag:V1Extra, V2:ClassTag:V2Extra](keysArray: Array[K], values1Array: Array[V1], values2Array: Array[V2]): M[K, V1, V2]

  def fromIterable[K:ClassTag:KExtra, V1:ClassTag:V1Extra, V2:ClassTag:V2Extra](kv1v2s: Iterable[(K, (V1, V2))]): M[K, V1, V2]

}
