package metal
package immutable

trait Map2[K, V1, V2] extends generic.Map2[K, V1, V2] with immutable.Collection { lhs =>

  type Immutable >: lhs.type <: immutable.Map2[K, V1, V2]

}
