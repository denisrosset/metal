package metal
package immutable

trait SetBuilder[K, SK <: immutable.Set[K]] extends generic.SetBuilder[K, SK] {

  type MK <: mutable.Set[K] { type Immutable <: SK }

  def mutableBuilder: mutable.SetBuilder[K, MK]

  def empty: SK = mutableBuilder.empty.result()

  def apply(items: K*): SK = mutableBuilder.apply(items: _*).result()

  def fromArray(array: Array[K]): SK = mutableBuilder.fromArray(array).result()

  def fromIterable(items: Iterable[K]): SK = mutableBuilder.fromIterable(items).result()

}
