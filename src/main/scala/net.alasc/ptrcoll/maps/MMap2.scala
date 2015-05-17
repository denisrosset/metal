package net.alasc.ptrcoll
package maps

import scala.{specialized => sp}
import scala.reflect.ClassTag
import scala.annotation.tailrec

import spire.algebra.Order
import spire.util.Opt

object MMap2 {
  @inline @tailrec final def isSubset[@sp(Int, Long) K, V1, V2](lm: MMap2[K, V1, V2], rm: MMap2[K, V1, V2])(lp: lm.Ptr): Boolean = lp.asInstanceOf[lm.Ptr] match {
    case Valid(lvp) =>
      val k = lm.ptrKey(lvp)
      rm.ptrFind(k) match {
        case Valid(rvp) if lm.ptrVal1(lvp) == rm.ptrVal1(rvp) && lm.ptrVal2(lvp) == rm.ptrVal2(rvp) => isSubset(lm, rm)(lm.ptrNext(lvp))
        case _ => false
      }
    case _ => true
  }

  @inline @tailrec final def hash[@sp(Int, Long) K, V1, V2](m: MMap2[K, V1, V2])(p: m.Ptr, h: Int): Int = p.asInstanceOf[m.Ptr] match {
    case Valid(vp) =>
      val k = m.ptrKey(vp)
      val v1 = m.ptrVal1(vp)
      val v2 = m.ptrVal2(vp)
      hash(m)(m.ptrNext(vp), h ^ k.## ^ (v1.## * 41 + v2.##))
    case _ => h
  }
}

trait MMap2[@sp(Int, Long) K, V1, V2] extends Searchable[K] with Countable with PointableValues1[V1] with PointableValues2[V2] { lhs =>
  implicit def ctK: ClassTag[K]
  implicit def ctV1: ClassTag[V1]
  implicit def ctV2: ClassTag[V2]

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
    case rhs: MMap2[K, V1, V2] if lhs.size == rhs.size && lhs.ctK == rhs.ctK && lhs.ctV1 == rhs.ctV1 && lhs.ctV2 == rhs.ctV2 => MMap2.isSubset(lhs, rhs)(lhs.ptrStart)
    case _ => false
  }

  /** Hashes the contents of the map to an Int value.
    * 
    * By xor'ing all the map's keys and values together, we can be sure
    * that maps with the same contents will have the same hashCode
    * regardless of the order those items appear.
    */
  override def hashCode: Int = MMap2.hash(lhs)(ptrStart, 0xDEADD065)

  /** Returns a string representation of the contents of the map.
    */
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("MMap(")
    @tailrec def rec(p: Ptr, prefix: String): Unit = p match {
      case Valid(vp) =>
        sb.append(prefix)
        sb.append(ptrKey(vp).toString)
      sb.append(" -> (")
      sb.append(ptrVal1(vp).toString)
      sb.append(", ")
      sb.append(ptrVal2(vp).toString)
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
    case Valid(vp) if ptrVal1(vp) == value1 && ptrVal2(vp) == value2 => true
    case _ => false
  }

  /** Returns the key's current value1 in the map, throwing an exception
    * if the key is not found.
    */
  def apply1(key: K): V1 = ptrFind(key) match {
    case Valid(vp) => ptrVal1(vp)
    case _ => throw new KeyNotFoundException(key.toString)
  }

  /** Returns the key's current value1 in the map, throwing an exception
    * if the key is not found.
    */
  def apply2(key: K): V2 = ptrFind(key) match {
    case Valid(vp) => ptrVal2(vp)
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
    case Valid(vp) => ptrVal1(vp)
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
    case Valid(vp) => ptrVal2(vp)
    case _ => fallback2
  }

  /** Returns the key's current value (1st part) in the map as an Opt, returning
    * Opt.empty if the key is not found.
    */
  def get1(key: K): Opt[V1] = ptrFind(key) match {
    case Valid(vp) => Opt(ptrVal1(vp))
    case _ => Opt.empty[V1]
  }

  /** Returns the key's current value (1st part) in the map as an Opt, returning
    * Opt.empty if the key is not found.
    */
  def get2(key: K): Opt[V2] = ptrFind(key) match {
    case Valid(vp) => Opt(ptrVal2(vp))
    case _ => Opt.empty[V2]
  }
}

trait MutMMap2[@sp(Int, Long) K, V1, V2] extends MMap2[K, V1, V2] with Removable[K] { lhs =>
  def copy: MutMMap2[K, V1, V2]

  /** Stores the value `(value1, value2)` for the key `key`.
    * 
    * If a previous value was associated with the key,
    * it is overwritten.
    * 
    */
  def update(key: K, value1: V1, value2: V2): Unit
}
