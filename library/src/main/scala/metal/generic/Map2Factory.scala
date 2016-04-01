package metal
package generic

trait Map2Factory {

  type KExtra[_]

  type V1Extra[_]

  type V2Extra[_]

  type M[K, V1, V2] <: generic.Map2[K, V1, V2]

  def empty[K:MetalTag:KExtra, V1:MetalTag:V1Extra, V2:MetalTag:V2Extra]: M[K, V1, V2]

  def apply[K:MetalTag:KExtra, V1:MetalTag:V1Extra, V2:MetalTag:V2Extra](kv1v2s: (K, (V1, V2))*): M[K, V1, V2]

  def fromMap[K:MetalTag:KExtra, V1:MetalTag:V1Extra, V2:MetalTag:V2Extra](map: scala.collection.Map[K, (V1, V2)]): M[K, V1, V2]

  def fromArrays[K:MetalTag:KExtra, V1:MetalTag:V1Extra, V2:MetalTag:V2Extra](keysArray: Array[K], values1Array: Array[V1], values2Array: Array[V2]): M[K, V1, V2]

  def fromIterable[K:MetalTag:KExtra, V1:MetalTag:V1Extra, V2:MetalTag:V2Extra](kv1v2s: Iterable[(K, (V1, V2))]): M[K, V1, V2]

}
