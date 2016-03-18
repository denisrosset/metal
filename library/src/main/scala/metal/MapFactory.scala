package metal

trait MapFactory {

  type KExtra[_]

  type VExtra[_]

  type M[K, V] <: metal.Map[K, V]

  def empty[K:Methods:KExtra, V:Methods:VExtra]: M[K, V]

  def apply[K:Methods:KExtra, V:Methods:VExtra](kvPairs: (K, V)*): M[K, V]

  def fromMap[K:Methods:KExtra, V:Methods:VExtra](map: scala.collection.Map[K, V]): M[K, V]

  def fromArrays[K:Methods:KExtra, V:Methods:VExtra](keysArray: Array[K], valuesArray: Array[V]): M[K, V]

}
