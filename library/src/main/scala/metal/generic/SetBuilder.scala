package metal
package generic

trait SetBuilder[K, SK <: generic.Set[K]] {

  def empty: SK

  def apply(items: K*): SK

  def fromArray(array: Array[K]): SK

  def fromIterable(items: Iterable[K]): SK

}
