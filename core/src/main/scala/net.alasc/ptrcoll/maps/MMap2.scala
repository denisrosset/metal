package net.alasc.ptrcoll
package maps

import scala.{specialized => sp}
import scala.reflect.ClassTag
import scala.annotation.tailrec

import spire.algebra.Order
import spire.util.Opt

object Map2 {

  @inline @tailrec final def isSubset[@sp(Int, Long) K, V1, V2](lm: Map2[K, V1, V2], rm: Map2[K, V1, V2])(lp: Ptr[lm.Tag]): Boolean = lp.asInstanceOf[Ptr[lm.Tag]] match {
    case VPtr(lvp) =>
      val k = lm.ptrKey(lvp)
      rm.ptrFind(k) match {
        case VPtr(rvp) if lm.ptrValue1(lvp) == rm.ptrValue1(rvp) && lm.ptrValue2(lvp) == rm.ptrValue2(rvp) => isSubset(lm, rm)(lm.ptrNext(lvp))
        case _ => false
      }
    case _ => true
  }

  @inline @tailrec final def hash[@sp(Int, Long) K, V1, V2](m: Map2[K, V1, V2])(p: Ptr[m.Tag], h: Int): Int = p.asInstanceOf[Ptr[m.Tag]] match {
    case VPtr(vp) =>
      val k = m.ptrKey(vp)
      val v1 = m.ptrValue1(vp)
      val v2 = m.ptrValue2(vp)
      hash(m)(m.ptrNext(vp), h ^ k.## ^ (v1.## * 41 + v2.##))
    case _ => h
  }

}

trait Map2[@sp(Int, Long) K, V1, V2] extends Searchable[K] with Countable with PointableValue1[V1] with PointableValue2[V2] { lhs =>

  implicit def ctK: ClassTag[K]

  implicit def ctV1: ClassTag[V1]

  implicit def ctV2: ClassTag[V2]

  def copy: Map2[K, V1, V2]

  /** Checks if two Map2 are equal.
    * 
    * Equal means the maps have the same types (which is checked
    * using the ClassTag instances) and the same contents.
    * 
    * Comparing Maps with any other collection types (including Scala's)
    *  will return false.
    */
  override def equals(rhs: Any): Boolean = rhs match {
    case rhs: Map2[K, V1, V2] if lhs.size == rhs.size && lhs.ctK == rhs.ctK && lhs.ctV1 == rhs.ctV1 && lhs.ctV2 == rhs.ctV2 => Map2.isSubset(lhs, rhs)(lhs.ptrStart)
    case _ => false
  }

  /** Hashes the contents of the map to an Int value.
    * 
    * By xor'ing all the map's keys and values together, we can be sure
    * that maps with the same contents will have the same hashCode
    * regardless of the order those items appear.
    */
  override def hashCode: Int = Map2.hash(lhs)(ptrStart, 0xDEADD065)

  /** Returns a string representation of the contents of the map.
    */
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("Map(")
    @tailrec def rec(p: Ptr[Tag], prefix: String): Unit = p match {
      case VPtr(vp) =>
        sb.append(prefix)
        sb.append(ptrKey(vp).toString)
      sb.append(" -> (")
      sb.append(ptrValue1(vp).toString)
      sb.append(", ")
      sb.append(ptrValue2(vp).toString)
        sb.append(")")
        rec(ptrNext(vp), ", ")
    }
    rec(ptrStart, "")
    sb.append(")")
    sb.toString
  }

  /** Returns whether the key is present in the Map with the given value
    * or not.
    */
  def containsItem(key: K, value1: V1, value2: V2): Boolean = ptrFind(key) match {
    case VPtr(vp) if ptrValue1(vp) == value1 && ptrValue2(vp) == value2 => true
    case _ => false
  }

  /** Returns the key's current value1 in the map, throwing an exception
    * if the key is not found.
    */
  def apply1(key: K): V1 = ptrFind(key) match {
    case VPtr(vp) => ptrValue1(vp)
    case _ => throw new KeyNotFoundException(key.toString)
  }

  /** Returns the key's current value1 in the map, throwing an exception
    * if the key is not found.
    */
  def apply2(key: K): V2 = ptrFind(key) match {
    case VPtr(vp) => ptrValue2(vp)
    case _ => throw new KeyNotFoundException(key.toString)
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
  def getOrElse1(key: K, fallback1: V1): V1 = ptrFind(key) match {
    case VPtr(vp) => ptrValue1(vp)
    case _ => fallback1
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
  def getOrElse2(key: K, fallback2: V2): V2 = ptrFind(key) match {
    case VPtr(vp) => ptrValue2(vp)
    case _ => fallback2
  }

  /** Returns the key's current value (1st part) in the map as an Opt, returning
    * Opt.empty if the key is not found.
    */
  def get1(key: K): Opt[V1] = ptrFind(key) match {
    case VPtr(vp) => Opt(ptrValue1(vp))
    case _ => Opt.empty[V1]
  }

  /** Returns the key's current value (1st part) in the map as an Opt, returning
    * Opt.empty if the key is not found.
    */
  def get2(key: K): Opt[V2] = ptrFind(key) match {
    case VPtr(vp) => Opt(ptrValue2(vp))
    case _ => Opt.empty[V2]
  }
}

trait MMap2[@sp(Int, Long) K, V1, V2] extends Map2[K, V1, V2] with Removable[K] { lhs =>
  def copy: MMap2[K, V1, V2]

  /** Stores the value `(value1, value2)` for the key `key`.
    * 
    * If a previous value was associated with the key,
    * it is overwritten.
    * 
    */
  def update(key: K, value1: V1, value2: V2): Unit
}
