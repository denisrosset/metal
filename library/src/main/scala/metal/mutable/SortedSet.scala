package metal.mutable

trait SortedSet[K] extends metal.SortedSet[K] with metal.mutable.Set[K] {

  type Immutable <: metal.immutable.SortedSet[K]
  type Mutable <: metal.mutable.SortedSet[K]

}
