package metal.immutable

trait Set[K] extends metal.Set[K] with metal.immutable.Collection {

  type Immutable >: this.type <: Set[K]

}
