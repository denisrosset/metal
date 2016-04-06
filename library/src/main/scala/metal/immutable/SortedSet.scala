package metal
package immutable

trait WrappedSortedSet[K] extends WrappedSet[K] {

  def w: metal.immutable.SortedSet[K]

}

trait SortedSet[K] extends generic.SortedSet[K] with immutable.Set[K] { lhs =>

  type Immutable >: lhs.type <: immutable.SortedSet[K]

}
