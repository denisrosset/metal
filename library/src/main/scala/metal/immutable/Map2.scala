package metal.immutable

trait Map2[K, V1, V2] extends metal.Map2[K, V1, V2] with metal.immutable.Collection {

  type Immutable >: this.type <: Map2[K, V1, V2]

}
