package net.alasc.ptrcoll
package maps

import scala.{specialized => sp}
import scala.reflect.ClassTag
import scala.annotation.tailrec

import spire.algebra.Order
import spire.util.Opt

trait MMap[@sp(Int, Long) K, V] extends Searchable[K] with Countable with PointableValues[V] { lhs =>
  implicit def ctK: ClassTag[K]
  implicit def ctV: ClassTag[V]

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
    case rhs: MMap[K, V] if lhs.size == rhs.size && lhs.ctK == rhs.ctK && lhs.ctV == rhs.ctV => MMap.isSubset(lhs, rhs)(lhs.ptrStart)
    case _ => false
  }
  /** Hashes the contents of the map to an Int value.
    * 
    * By xor'ing all the map's keys and values together, we can be sure
    * that maps with the same contents will have the same hashCode
    * regardless of the order those items appear.
    */
  override def hashCode: Int = MMap.hash(lhs)(ptrStart, 0xDEADD065)

  /** Returns a string representation of the contents of the map.
    */
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("MMap(")
    @tailrec def rec(p: Ptr, prefix: String): Unit = p match {
      case Valid(vp) =>
        sb.append(prefix)
        sb.append(ptrKey(vp).toString)
        sb.append(" -> ")
        sb.append(ptrVal(vp).toString)
        rec(ptrNext(vp), ", ")
      case _ =>
    }
    rec(ptrStart, "")
    sb.append(")")
    sb.toString
  }

  /** Returns whether the key is present in the Map with the given value
    * or not.
    */
  def containsItem(key: K, value: V): Boolean

  /** Returns the key's current value in the map, throwing an exception
    * if the key is not found.
    */
  def apply(key: K): V

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
  def getOrElse(key: K, fallback: V): V

  /** Returns the key's current value in the map as an Opt, returning
    * Opt.empty if the key is not found.
    */
  def get(key: K): Opt[V]
}

trait MutMMap[@sp(Int, Long) K, V] extends MMap[K, V] with Removable[K] { lhs =>
  def copy: MutMMap[K, V]

  /** Stores the value `value` for the key `key`.
    * 
    * If a previous value was associated with the key,
    * it is overwritten.
    * 
    * This method is usually invoked as map(key) = value, but can also
    * be invoked as map.update(key, value).
    */
  def update(key: K, value: V): Unit
}

object MMap {
  @inline @tailrec final def isSubset[@sp(Int, Long) K, V](lm: MMap[K, V], rm: MMap[K, V])(lp: lm.Ptr): Boolean = lp.asInstanceOf[lm.Ptr] match {
    case Valid(lvp) =>
      val k = lm.ptrKey(lvp)
      rm.ptrFind(k) match {
        case Valid(rvp) if lm.ptrVal(lvp) == rm.ptrVal(rvp) => isSubset(lm, rm)(lm.ptrNext(lvp))
        case _ => false
      }
    case _ => true
  }

  @inline @tailrec final def hash[@sp(Int, Long) K, V](m: MMap[K, V])(p: m.Ptr, h: Int): Int = p.asInstanceOf[m.Ptr] match {
      case Valid(vp) =>
        val k = m.ptrKey(vp)
        val v = m.ptrVal(vp)
        hash(m)(m.ptrNext(vp), h ^ k.## ^ v.##)
      case _ => h
    }
}
