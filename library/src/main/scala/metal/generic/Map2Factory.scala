package metal
package generic

trait Map2Factory {

  type KExtra[_]

  type V1Extra[_]

  type V2Extra[_]

  type M[K, V1, V2] <: generic.Map2[K, V1, V2]

  def empty[K:Methods:KExtra, V1:Methods:V1Extra, V2:Methods:V2Extra]: M[K, V1, V2]

  def apply[K:Methods:KExtra, V1:Methods:V1Extra, V2:Methods:V2Extra](kv1v2s: (K, (V1, V2))*): M[K, V1, V2]

  def fromMap[K:Methods:KExtra, V1:Methods:V1Extra, V2:Methods:V2Extra](map: scala.collection.Map[K, (V1, V2)]): M[K, V1, V2]

  def fromArrays[K:Methods:KExtra, V1:Methods:V1Extra, V2:Methods:V2Extra](keysArray: Array[K], values1Array: Array[V1], values2Array: Array[V2]): M[K, V1, V2]

  def fromIterable[K:Methods:KExtra, V1:Methods:V1Extra, V2:Methods:V2Extra](kv1v2s: Iterable[(K, (V1, V2))]): M[K, V1, V2]

}
