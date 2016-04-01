package metal
package generic

trait MapFactory {

  type KExtra[_]

  type VExtra[_]

  type M[K, V] <: generic.Map[K, V]

  def empty[K:MetalTag:KExtra, V:MetalTag:VExtra]: M[K, V]

  def apply[K:MetalTag:KExtra, V:MetalTag:VExtra](kvPairs: (K, V)*): M[K, V]

  def fromMap[K:MetalTag:KExtra, V:MetalTag:VExtra](map: scala.collection.Map[K, V]): M[K, V]

  def fromArrays[K:MetalTag:KExtra, V:MetalTag:VExtra](keysArray: Array[K], valuesArray: Array[V]): M[K, V]

  def fromIterable[K:MetalTag:KExtra, V:MetalTag:VExtra](keyValuePairs: Iterable[(K, V)]): M[K, V]

}
