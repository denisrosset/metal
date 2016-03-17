package metal.mutable

import metal.{AddKeys, Removable, Updatable}

trait Map[K, V] extends metal.Map[K, V] with metal.mutable.Collection with Removable with AddKeys[K] with Updatable[V] {

  type Immutable <: metal.immutable.Map[K, V]
  type Mutable <: metal.mutable.Map[K, V]

}
