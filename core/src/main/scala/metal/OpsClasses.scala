package metal

import spire.util.Opt

final class SearchableOps[K](val lhs: Searchable[K]) extends AnyVal {

  /** Returns whether `key` is present in the collection. */
  @inline final def contains(key: K): Boolean = macro OpsMacros.contains[K]

}

final class RemovableSearchableOps[K, T <: Searchable[K] with Removable[K]](val lhs: T) extends AnyVal {

  /** Removes any value associated with key, and returns whether
    * an operation was performed.
    */
  @inline final def remove(key: K): Boolean = macro OpsMacros.remove[K]

  /** Removes key from collection. */
  @inline final def -=(key: K): T = macro OpsMacros.-=[K, T]

  /*
   final def --=(coll: Nextable with Countable with PointableKey[K]): lhs.type = {
   @tailrec def rec(p: Ptr[coll.Tag]): Unit = p.asInstanceOf[Ptr[coll.Tag]] match {
   case VPtr(vp) =>
   lhs.remove(vp.key)
   rec(vp.next)
   case _ =>
   }
   rec(coll.ptrStart)
   lhs
   }*/

}

final class AddOps[K, T <: AddKeys[K] with NoValues](val lhs: T) extends AnyVal {

  /** Adds item to the set, and returns the set. */
  @inline final def +=(key: K): T =  macro OpsMacros.+=[K, T]

}

final class SearchableAddOps[K](val lhs: AddKeys[K] with Searchable[K] with NoValues) extends AnyVal {

  /**
    * Adds item to the set.
    * 
    * Returns whether or not the item was added. If item was already in
    * the set, this method will do nothing and return false.
    */
  @inline final def add(key: K): Boolean = macro OpsMacros.add[K]

}

final class UpdateOps[K, V](val lhs: AddKeys[K] with Updatable[V]) extends AnyVal {

  /** Stores the value `value` for the key `key`.
    * 
    * If a previous value was associated with the key,
    * it is overwritten.
    * 
    * This method is usually invoked as map(key) = value, but can also
    * be invoked as map.update(key, value).
    */
  @inline final def update(key: K, value: V): Unit = macro OpsMacros.update[K, V]

}

final class SearchableValuesOps[K, V](val lhs: Searchable[K] with Values[V]) extends AnyVal {

  /** Returns whether the key is present in the Map with the given value
    * or not.
    */
  @inline final def containsItem(key: K, value: V): Boolean = macro OpsMacros.containsItem[K, V]

  /** Returns the key's current value in the map, throwing an exception
    * if the key is not found.
    */
  @inline final def apply(key: K): V = macro OpsMacros.mapply[K, V]

  /** Returns the key's current value in the map, returning the given
    * fallback value if the key is not found.
    * 
    * Unlike Scala's method, this method is eager in its second
    * parameters, so it should only be used if the default value is
    * already available (or a literal, or very cheap).
    * 
    * In cases where a lazy parameter would be desired, you should use
    * something like: myMap.get(key).getOrElse(default).
    */
  @inline final def getOrElse(key: K, fallback: V): V = macro OpsMacros.getOrElse[K, V]

  /** Returns the key's current value in the map as an Opt, returning
    * Opt.empty if the key is not found.
    */
  @inline final def get(key: K): Opt[V] = macro OpsMacros.get[K, V]

}
