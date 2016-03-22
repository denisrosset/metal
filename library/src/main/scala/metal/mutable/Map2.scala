package metal
package mutable

trait Map2[K, V1, V2]
    extends generic.Map2[K, V1, V2]
    with mutable.Collection
    with Removable
    with AddKeys[K]
    with Updatable1[V1]
    with Updatable2[V2]
