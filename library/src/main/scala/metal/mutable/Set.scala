package metal.mutable

import metal.{AddKeys, Removable}

trait Set[K] extends metal.Set[K] with metal.mutable.Collection with Removable with AddKeys[K]
