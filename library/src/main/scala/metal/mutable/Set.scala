package metal.mutable

import metal.{AddKeys, Removable}

trait Set[K] extends metal.Set[K] with metal.mutable.Collection with Removable with AddKeys[K] {

  type Immutable <: metal.immutable.Set[K]
  type Mutable <: metal.mutable.Set[K]

}
