package metal
package immutable

trait Set[K] extends generic.Set[K] with immutable.Collection { lhs =>

  type Immutable >: lhs.type <: Set[K]

}
