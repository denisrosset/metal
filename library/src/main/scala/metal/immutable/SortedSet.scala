package metal
package immutable

trait SortedSet[K] extends generic.SortedSet[K] with immutable.Set[K] { lhs =>

  type Immutable >: lhs.type <: immutable.SortedSet[K]

}
