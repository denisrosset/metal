package metal
package immutable

trait WrappedSet[K] {

  def w: metal.immutable.Set[K]

}

trait Set[K] extends generic.Set[K] with immutable.Collection { lhs =>

  type Immutable >: lhs.type <: Set[K]

}
