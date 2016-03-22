package metal
package mutable

trait Set[K] extends generic.Set[K] with mutable.Collection with Removable with AddKeys[K]
