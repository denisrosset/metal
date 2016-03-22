package metal
package immutable

trait Collection extends generic.Collection { lhs =>

  type Immutable >: lhs.type <: Collection

  def toImmutable = lhs

}
