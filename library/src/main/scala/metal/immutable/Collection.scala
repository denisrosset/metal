package metal.immutable

trait Collection extends metal.Collection { lhs =>

  type Immutable >: lhs.type <: Collection

  def toImmutable = lhs

}
