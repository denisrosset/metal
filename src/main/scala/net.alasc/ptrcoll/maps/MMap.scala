package net.alasc.ptrcoll
package maps

import scala.{specialized => sp}
import scala.reflect.ClassTag

import spire.algebra.Order
import spire.util.Opt

import syntax.all._

trait MMapBase[@sp(Int, Long) K] extends Pointable with KeysRemovable[K] with Sized { self =>
  implicit def ctK: ClassTag[K]
  implicit def PtrTC: HasPtrAt[K, Ptr]
}

trait MMap[@sp(Int, Long) K, V] extends MMapBase[K] { lhs =>
  implicit def ctV: ClassTag[V]
  implicit def PtrTC: HasPtrAt[K, Ptr] with HasPtrVal[V, Ptr]

  def copy: MMap[K, V]

  /** Checks if two MMaps are equal.
    * 
    * Equal means the maps have the same types (which is checked
    * using the ClassTag instances) and the same contents.
    * 
    * Comparing Maps with any other collection types (including Scala's)
    *  will return false.
    */
  override def equals(rhs: Any): Boolean = rhs match {
    case rhs: MMap[_, _] =>
      if (lhs.size != rhs.size || lhs.ctK != rhs.ctK || lhs.ctV != rhs.ctV) return false
      val m: MMap[K, V] = rhs.asInstanceOf[MMap[K, V]]
      import m.{PtrTC => mTC}
      var p = lhs.pointer
      while (p.hasAt) {
        val k = p.at
        val v = p.atVal
        val rhsPtr = m.findPointerAt(k)
        if (!rhsPtr.hasAt) return false
        if (rhsPtr.atVal != v) return false
        p = p.next
      }
      true
    case _ => false
  }

  /** Hashes the contents of the map to an Int value.
    * 
    * By xor'ing all the map's keys and values together, we can be sure
    * that maps with the same contents will have the same hashCode
    * regardless of the order those items appear.
    */
  override def hashCode: Int = {
    var hash: Int = 0xDEADD065
    var p = lhs.pointer
    while (p.hasAt) {
      val k = p.at
      val v = p.atVal
      hash ^= (k.## ^ v.##)
      p = p.next
    }
    hash
  }

  /** Returns a string representation of the contents of the map.
    */
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("MMap(")
    var prefix = ""
    var p = pointer
    while (p.hasAt) {
      sb.append(prefix)
      prefix = ", "
      sb.append(p.at.toString)
      sb.append(" -> ")
      sb.append(p.atVal.toString)
      p = p.next
    }
    sb.append(")")
    sb.toString
  }

  /** Stores the value `value` for the key `key`.
    * 
    * If a previous value was associated with the key,
    * it is overwritten.
    * 
    * This method is usually invoked as map(key) = value, but can also
    * be invoked as map.update(key, value).
    */
  def update(key: K, value: V): Unit

  /** Returns whether the key is present in the Map with the given value
    * or not.
    */
  def containsItem(key: K, value: V): Boolean = {
    val ptr = findPointerAt(key)
    if (ptr.hasAt) (ptr.atVal == value) else false
  }

  /** Returns the key's current value in the map, throwing an exception
    * if the key is not found.
    */
  def apply(key: K): V = {
    val ptr = findPointerAt(key)
    if (ptr.hasAt) ptr.atVal else throw new KeyNotFoundException(key.toString)
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
  def getOrElse(key: K, fallback: V): V = {
    val ptr = findPointerAt(key)
    if (ptr.hasAt) ptr.atVal else fallback
  }

  /** Returns the key's current value in the map as an Opt, returning
    * Opt.empty if the key is not found.
    */
  def get(key: K): Opt[V] = {
    val ptr = findPointerAt(key)
    if (ptr.hasAt) Opt(ptr.atVal) else Opt.empty[V]
  }
}

trait MMap2[@sp(Int, Long) K, V1, V2] extends MMapBase[K] { lhs =>
  implicit def ctV1: ClassTag[V1]
  implicit def ctV2: ClassTag[V2]
  implicit def PtrTC: HasPtrAt[K, Ptr] with HasPtrVal1[V1, Ptr] with HasPtrVal2[V2, Ptr]

  def copy: MMap2[K, V1, V2]

  /** Checks if two MMap2 are equal.
    * 
    * Equal means the maps have the same types (which is checked
    * using the ClassTag instances) and the same contents.
    * 
    * Comparing Maps with any other collection types (including Scala's)
    *  will return false.
    */
  override def equals(rhs: Any): Boolean = rhs match {
    case rhs: MMap2[_, _, _] =>
      if (lhs.size != rhs.size || lhs.ctK != rhs.ctK ||
        lhs.ctV1 != rhs.ctV1 || lhs.ctV2 != rhs.ctV2) return false
      val m: MMap2[K, V1, V2] = rhs.asInstanceOf[MMap2[K, V1, V2]]
      import m.{PtrTC => mTC}
      var p = lhs.pointer
      while (p.hasAt) {
        val k = p.at
        val v1 = p.atVal1
        val v2 = p.atVal2
        val rhsPtr = m.findPointerAt(k)
        if (!rhsPtr.hasAt) return false
        if (rhsPtr.atVal1 != v1) return false
        if (rhsPtr.atVal2 != v2) return false
        p = p.next
      }
      true
    case _ => false
  }

  /** Hashes the contents of the map to an Int value.
    * 
    * By xor'ing all the map's keys and values together, we can be sure
    * that maps with the same contents will have the same hashCode
    * regardless of the order those items appear.
    */
  override def hashCode: Int = {
    var hash: Int = 0xDEADD065
    var p = lhs.pointer
    while (p.hasAt) {
      val k = p.at
      val v1 = p.atVal1
      val v2 = p.atVal2
      hash ^= (k.## ^ (v1.## * 41 + v2.##))
      p = p.next
    }
    hash
  }

  /** Returns a string representation of the contents of the map.
    */
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("MMap(")
    var prefix = ""
    var p = pointer
    while (p.hasAt) {
      sb.append(prefix)
      prefix = ", "
      sb.append(p.at.toString)
      sb.append(" -> (")
      sb.append(p.atVal1.toString)
      sb.append(", ")
      sb.append(p.atVal2.toString)
      sb.append(")")
      p = p.next
    }
    sb.append(")")
    sb.toString
  }

  /** Stores the value `(value1, value2)` for the key `key`.
    * 
    * If a previous value was associated with the key,
    * it is overwritten.
    * 
    */
  def update(key: K, value1: V1, value2: V2): Unit

  /** Returns whether the key is present in the Map with the given value
    * or not.
    */
  def containsItem(key: K, value1: V1, value2: V2): Boolean = {
    val ptr = findPointerAt(key)
    if (ptr.hasAt) (ptr.atVal1 == value1 && ptr.atVal2 == value2) else false
  }

  /** Returns the key's current value1 in the map, throwing an exception
    * if the key is not found.
    */
  def apply1(key: K): V1 = {
    val ptr = findPointerAt(key)
    if (ptr.hasAt) ptr.atVal1 else throw new KeyNotFoundException(key.toString)
  }

  /** Returns the key's current value1 in the map, throwing an exception
    * if the key is not found.
    */
  def apply2(key: K): V2 = {
    val ptr = findPointerAt(key)
    if (ptr.hasAt) ptr.atVal2 else throw new KeyNotFoundException(key.toString)
  }

  /** Returns the key's current value (1st part) in the map, 
    * returning the given fallback value part if the key is not found.
    * 
    * Unlike Scala's method, this method is eager in its second
    * parameters, so it should only be used if the default value is
    * already available (or a literal, or very cheap).
    * 
    * In cases where a lazy parameter would be desired, you should use
    * something like: myMap.get1(key).getOrElse(default).
    */
  def getOrElse1(key: K, fallback1: V1): V1 = {
    val ptr = findPointerAt(key)
    if (ptr.hasAt) ptr.atVal1 else fallback1
  }

  /** Returns the key's current value (2nd part) in the map,
    * returning the given fallback value part if the key is not found.
    * 
    * Unlike Scala's method, this method is eager in its second
    * parameters, so it should only be used if the default value is
    * already available (or a literal, or very cheap).
    * 
    * In cases where a lazy parameter would be desired, you should use
    * something like: myMap.get2(key).getOrElse(default).
    */
  def getOrElse2(key: K, fallback2: V2): V2 = {
    val ptr = findPointerAt(key)
    if (ptr.hasAt) ptr.atVal2 else fallback2
  }

  /** Returns the key's current value (1st part) in the map as an Opt, returning
    * Opt.empty if the key is not found.
    */
  def get1(key: K): Opt[V1] = {
    val ptr = findPointerAt(key)
    if (ptr.hasAt) Opt(ptr.atVal1) else Opt.empty[V1]
  }

  /** Returns the key's current value (1st part) in the map as an Opt, returning
    * Opt.empty if the key is not found.
    */
  def get2(key: K): Opt[V2] = {
    val ptr = findPointerAt(key)
    if (ptr.hasAt) Opt(ptr.atVal2) else Opt.empty[V2]
  }
}
