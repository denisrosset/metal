package metal
package generic

import scala.reflect.ClassTag

trait MapFactory {

  type KExtra[_]

  type VExtra[_]

  type M[K, V] <: generic.Map[K, V]

  def empty[K:ClassTag:KExtra, V:ClassTag:VExtra]: M[K, V]

  def apply[K:ClassTag:KExtra, V:ClassTag:VExtra](kvPairs: (K, V)*): M[K, V]

  def fromMap[K:ClassTag:KExtra, V:ClassTag:VExtra](map: scala.collection.Map[K, V]): M[K, V]

  def fromArrays[K:ClassTag:KExtra, V:ClassTag:VExtra](keysArray: Array[K], valuesArray: Array[V]): M[K, V]

  def fromIterable[K:ClassTag:KExtra, V:ClassTag:VExtra](keyValuePairs: Iterable[(K, V)]): M[K, V]

}
