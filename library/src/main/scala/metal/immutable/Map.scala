package metal
package immutable

trait WrappedMap[K, V] {

  def w: metal.immutable.Map[K, V]

}

trait Map[K, V] extends generic.Map[K, V] with immutable.Collection {

  type Immutable >: this.type <: metal.immutable.Map[K, V]

}
