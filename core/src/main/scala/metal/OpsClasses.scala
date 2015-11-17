package metal

import spire.util.Opt

final class SearchableOps[K](val lhs: Searchable[K]) {

  /** Returns whether `key` is present in the container. */
  def contains(key: K): Boolean = macro macros.Ops.contains[K]

}

final class RemovableSearchableOps[K, T <: Searchable[K] with Removable](val lhs: T) {

  /** Removes any value associated with key, and returns whether
    * an operation was performed.
    */
  def remove(key: K): Boolean = macro macros.Ops.remove[K]

  /** Removes key from container. */
  def -=(key: K): T = macro macros.Ops.-=[K, T]

  /*
   final def --=(coll: Nextable with Countable with PointableKey[K]): lhs.type = {
   @tailrec def rec(p: Ptr[coll.Tag]): Unit = p.asInstanceOf[Ptr[coll.Tag]] match {
   case VPtr(vp) =>
   lhs.remove(vp.key)
   rec(vp.next)
   case _ =>
   }
   rec(coll.ptr)
   lhs
   }*/

}

final class AddOps[K, T <: AddKeys[K] with Elements[K]](val lhs: T) {

  /** Adds item to the set, and returns the set. */
  def +=(key: K): T =  macro macros.Ops.+=[K, T]

}

final class SearchableAddOps[K](val lhs: AddKeys[K] with Searchable[K] with Elements[K]) {

  /**
    * Adds item to the set.
    * 
    * Returns whether or not the item was added. If item was already in
    * the set, this method will do nothing and return false.
    */
  def add(key: K): Boolean = macro macros.Ops.add[K]

}

final class UpdateOps[K, V](val lhs: AddKeys[K] with Updatable[V]) {

  /** Stores the value `value` for the key `key`.
    * 
    * If a previous value was associated with the key,
    * it is overwritten.
    * 
    * This method is usually invoked as map(key) = value, but can also
    * be invoked as map.update(key, value).
    */
  def update(key: K, value: V): Unit = macro macros.Ops.update[K, V]

}
/*
final class Update2Ops[K, V1, V2](val lhs: AddKeys[K] with Updatable1[V1] with Updatable2[V2]) {

  /** Stores the values `value1`, `value2` for the key `key`.
    * 
    * If previous values were associated with the key,
    * they are overwritten.
    */
  def update(key: K, value: (V1, V2)): Unit = macro macros.Ops.update2[K, V1, V2]

}
 */
final class SearchableValuesOps[K, V](val lhs: Searchable[K] with Values[V]) {

  /** Returns whether the key is present in the Map with the given value
    * or not.
    */
  def containsItem(key: K, value: V): Boolean = macro macros.Ops.containsItem[K, V]

  /** Returns the key's current value in the map, throwing an exception
    * if the key is not found.
    */
  def apply(key: K): V = macro macros.Ops.apply[K, V]

  /** Returns the key's current value in the map, returning the given
    * fallback value if the key is not found.
    */
  def getOrElse(key: K, fallback: V): V = macro macros.Ops.getOrElse[K, V]

  /** Returns the key's current value in the map as an Opt, returning
    * Opt.empty if the key is not found.
    */
  def get(key: K): Opt[V] = macro macros.Ops.get[K, V]

}

/*
final class SearchableValues1Ops[K, V1](val lhs: Searchable[K] with Values1[V1]) {

  /** Returns the key's current value in the map, throwing an exception
    * if the key is not found.
    */
  def apply1(key: K): V1 = macro macros.Ops.apply1[K, V1]

  /** Returns the key's current value in the map, returning the given
    * fallback value if the key is not found.
    */
  def getOrElse1(key: K, fallback: V1): V1 = macro macros.Ops.getOrElse1[K, V1]

  /** Returns the key's current value in the map as an Opt, returning
    * Opt.empty if the key is not found.
    */
  def get1(key: K): Opt[V1] = macro macros.Ops.get1[K, V1]

}

final class SearchableValues2Ops[K, V2](val lhs: Searchable[K] with Values2[V2]) {

  /** Returns the key's current value in the map, throwing an exception
    * if the key is not found.
    */
  def apply2(key: K): V2 = macro macros.Ops.apply2[K, V2]

  /** Returns the key's current value in the map, returning the given
    * fallback value if the key is not found.
    */
  def getOrElse2(key: K, fallback: V2): V2 = macro macros.Ops.getOrElse2[K, V2]

  /** Returns the key's current value in the map as an Opt, returning
    * Opt.empty if the key is not found.
    */
  def get2(key: K): Opt[V2] = macro macros.Ops.get2[K, V2]

}
 */
