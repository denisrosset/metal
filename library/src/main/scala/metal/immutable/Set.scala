package metal.immutable

import spire.util.Opt

trait Set[K] extends metal.Set[K] with metal.immutable.Collection {

  type Immutable >: this.type <: Set[K]
  type Mutable <: metal.mutable.Set[K]

}
