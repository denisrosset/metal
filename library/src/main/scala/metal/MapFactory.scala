package metal

import scala.reflect.ClassTag

trait MapFactory {

  type KExtra[_]

  type M[K, V] <: metal.Map[K, V]

  def empty[K:Methods:KExtra, V:Methods]: M[K, V]

  def apply[K:Methods:KExtra, V:Methods](kvPairs: (K, V)*): M[K, V]

  def fromMap[K:Methods:KExtra, V:Methods](map: scala.collection.Map[K, V]): M[K, V]

  def fromArrays[K:Methods:KExtra, V:Methods](keysArray: Array[K], valuesArray: Array[V]): M[K, V]

}
