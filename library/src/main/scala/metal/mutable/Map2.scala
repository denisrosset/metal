package metal.mutable

import metal.{AddKeys, Removable, Updatable1, Updatable2}

trait Map2[K, V1, V2] extends metal.Map2[K, V1, V2] with metal.mutable.Collection with Removable with AddKeys[K] with Updatable1[V1] with Updatable2[V2]
