package metal

import spire.util.Opt

final class SearchableOps[K](val lhs: Searchable[K]) extends AnyVal {

  /** Returns whether `key` is present in the collection. */
  @inline final def contains(key: K): Boolean = lhs.ptrFind(key).nonNull

}

final class RemovableSearchableOps[K](val lhs: Searchable[K] with Removable[K]) {

  /** Removes any value associated with key, and returns whether
    * an operation was performed.
    */
  @inline final def remove(key: K): Boolean = lhs.ptrFind(key) match {
    case VPtr(vp) =>
      lhs.ptrRemove(vp)
      true
    case _ => false
  }

  /** Removes key from collection. */
  @inline final def -=(key: K): lhs.type = lhs.ptrFind(key) match {
    case VPtr(vp) =>
      lhs.ptrRemove(vp)
      lhs
    case _ => lhs
  }

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

final class AddOps[K, T <: AddKeys[K] with NoValues](val lhs: T) {

  /** Adds item to the set, and returns the set. */
    def +=(key: K): T = {
      lhs.ptrAddKey(key)
      lhs
    }

}

final class SearchableAddOps[K](val lhs: AddKeys[K] with Searchable[K] with NoValues) {

  /**
    * Adds item to the set.
    * 
    * Returns whether or not the item was added. If item was already in
    * the set, this method will do nothing and return false.
    */
  def add(key: K): Boolean = {
    val contained = lhs.ptrFind(key).nonNull
    lhs.ptrAddKey(key)
    contained
  }

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
  def update(key: K, value: V): Unit = lhs.ptrUpdate(lhs.ptrAddKey(key), value)

}

final class SearchableValuesOps[K, V](val lhs: Searchable[K] with Values[V]) {

  /** Returns whether the key is present in the Map with the given value
    * or not.
    */
  def containsItem(key: K, value: V): Boolean = lhs.ptrFind(key) match {
    case VPtr(vp) => lhs.ptrValue(vp) == value
    case _ => false
  }

  /** Returns the key's current value in the map, throwing an exception
    * if the key is not found.
    */
  def apply(key: K): V = lhs.ptrFind(key) match {
    case VPtr(vp) => lhs.ptrValue(vp)
    case _ => throw new NoSuchElementException("key not found: " + key)
  }

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
  // TODO: replace by macro so lazy/eager does not make sense
  def getOrElse(key: K, fallback: V): V = lhs.ptrFind(key) match {
    case VPtr(vp) => lhs.ptrValue(vp)
    case _ => fallback
  }

  /** Returns the key's current value in the map as an Opt, returning
    * Opt.empty if the key is not found.
    */
  def get(key: K): Opt[V] = lhs.ptrFind(key) match {
    case VPtr(vp) =>
      val v: V = lhs.ptrValue(vp)
      Opt[V](v)
    case _ => Opt.empty[V]
  }

}
