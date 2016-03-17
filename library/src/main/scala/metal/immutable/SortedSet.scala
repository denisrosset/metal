package metal.immutable

trait SortedSet[K] extends metal.SortedSet[K] with metal.immutable.Set[K] {

  type Immutable >: this.type <: metal.immutable.SortedSet[K]
  type Mutable <: metal.mutable.SortedSet[K]

}
