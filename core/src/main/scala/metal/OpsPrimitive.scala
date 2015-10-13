package metal

import spire.util.Opt

final class SearchableOpsP[K](val lhs: Searchable[K])(implicit ev: Primitive[K]) {

  /** Returns whether `key` is present in the collection. */
  final def contains(key: K): Boolean = macro OpsMacrosP.contains[K]

}

final class RemovableSearchableOpsP[K](val lhs: Searchable[K] with Removable[K])(implicit ev: Primitive[K]) {

  final def remove(key: K): Boolean = macro OpsMacrosP.remove[K]

  final def -=(key: K): lhs.type = macro OpsMacrosP.-=[K, lhs.type]
}

final class AddOpsP[K, T <: AddKeys[K] with NoValues](val lhs: T)(implicit ev: Primitive[K]) {

  final def +=(key: K): T = macro OpsMacrosP.+=[K, T]

}

final class SearchableAddOpsP[K](val lhs: AddKeys[K] with Searchable[K] with NoValues)(implicit ev: Primitive[K]) {

  /**
    * Adds item to the set.
    * 
    * Returns whether or not the item was added. If item was already in
    * the set, this method will do nothing and return false.
    */
  def add(key: K): Boolean = macro OpsMacrosP.add[K]

}

final class UpdateOpsP[K, V](val lhs: AddKeys[K] with Updatable[V])(implicit evK: Primitive[K], evV: Primitive[V]) {

  /** Removes key from collection. */
  def update(key: K, value: V): Unit = macro OpsMacrosP.update[K, V]

}

final class SearchableValuesOpsP[K, V](val lhs: Searchable[K] with Values[V])(implicit evK: Primitive[K], evV: Primitive[V]) {

  def containsItem(key: K, value: V): Boolean = macro OpsMacrosP.containsItem[K, V]

  def apply(key: K): V = macro OpsMacrosP.apply[K, V]

  def getOrElse(key: K, fallback: V): V = macro OpsMacrosP.getOrElse[K, V]

  def get(key: K): Opt[V] = macro OpsMacrosP.get[K, V]

}
