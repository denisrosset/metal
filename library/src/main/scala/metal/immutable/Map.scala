package metal.immutable

trait Map[K, V] extends metal.Map[K, V] with metal.immutable.Collection {

  type Immutable >: this.type <: Map[K, V]

}
