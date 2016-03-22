package metal
package mutable


trait Map[K, V] extends generic.Map[K, V] with mutable.Collection with Removable with AddKeys[K] with Updatable[V]
