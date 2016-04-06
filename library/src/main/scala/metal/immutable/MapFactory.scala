package metal
package immutable

import scala.reflect.ClassTag

trait MapFactory extends generic.MapFactory {

  type M[K, V] <: immutable.Map[K, V]
  type MM[K, V] <: mutable.Map[K, V] { type Immutable <: M[K, V] }

  def mutableFactory: mutable.MapFactory { type M[K, V] = MM[K, V]; type KExtra[K] = MapFactory.this.KExtra[K]; type VExtra[V] = MapFactory.this.VExtra[V] }

  def empty[K:ClassTag:KExtra, V:ClassTag:VExtra]: M[K, V] = mutableFactory.empty[K, V].result()

  def apply[K:ClassTag:KExtra, V:ClassTag:VExtra](kvPairs: (K, V)*): M[K, V] = mutableFactory.apply(kvPairs: _*).result()

  def fromMap[K:ClassTag:KExtra, V:ClassTag:VExtra](map: scala.collection.Map[K, V]): M[K, V] = mutableFactory.fromMap(map).result()

  def fromArrays[K:ClassTag:KExtra, V:ClassTag:VExtra](keysArray: Array[K], valuesArray: Array[V]): M[K, V] = mutableFactory.fromArrays(keysArray, valuesArray).result()

  def fromIterable[K:ClassTag:KExtra, V:ClassTag:VExtra](keyValuePairs: Iterable[(K, V)]): M[K, V] = mutableFactory.fromIterable(keyValuePairs).result()

}
