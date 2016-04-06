package metal
package immutable

import scala.reflect.ClassTag

trait Map2Factory extends generic.Map2Factory {

  type M[K, V1, V2] <: immutable.Map2[K, V1, V2]
  type MM[K, V1, V2] <: mutable.Map2[K, V1, V2] { type Immutable <: M[K, V1, V2] }

  def mutableFactory: mutable.Map2Factory { type M[K, V1, V2] = MM[K, V1, V2]; type KExtra[K] = Map2Factory.this.KExtra[K]; type V1Extra[V1] = Map2Factory.this.V1Extra[V1]; type V2Extra[V2] = Map2Factory.this.V2Extra[V2] }

  def empty[K:ClassTag:KExtra, V1:ClassTag:V1Extra, V2:ClassTag:V2Extra]: M[K, V1, V2] =
    mutableFactory.empty[K, V1, V2].result()

  def apply[K:ClassTag:KExtra, V1:ClassTag:V1Extra, V2:ClassTag:V2Extra](kv1v2s: (K, (V1, V2))*): M[K, V1, V2] =
    mutableFactory.apply(kv1v2s: _*).result()

  def fromMap[K:ClassTag:KExtra, V1:ClassTag:V1Extra, V2:ClassTag:V2Extra](map: scala.collection.Map[K, (V1, V2)]): M[K, V1, V2] =
    mutableFactory.fromMap(map).result()

  def fromArrays[K:ClassTag:KExtra, V1:ClassTag:V1Extra, V2:ClassTag:V2Extra](keysArray: Array[K], values1Array: Array[V1], values2Array: Array[V2]): M[K, V1, V2] =
    mutableFactory.fromArrays(keysArray, values1Array, values2Array).result()

  def fromIterable[K:ClassTag:KExtra, V1:ClassTag:V1Extra, V2:ClassTag:V2Extra](kv1v2s: Iterable[(K, (V1, V2))]): M[K, V1, V2] = mutableFactory.fromIterable(kv1v2s).result()

}
