package metal

import scala.reflect.ClassTag
import scala.annotation.tailrec

import spire.algebra.Order
import spire.util.Opt

trait Map[K, V] extends Searchable[K] with Countable with Values[V] { lhs =>

  implicit def ctK: ClassTag[K]

  implicit def ctV: ClassTag[V]

  def copy: Map[K, V]

  /** Checks if two Maps are equal.
    * 
    * Equal means the maps have the same types (which is checked
    * using the ClassTag instances) and the same contents.
    * 
    * Comparing Maps with any other collection types (including Scala's)
    *  will return false.
    */
  override def equals(rhs: Any): Boolean = rhs match {
    case rhs: Map[K, V] if lhs.size == rhs.size && lhs.ctK == rhs.ctK && lhs.ctV == rhs.ctV => Map.isSubset(lhs, rhs)(lhs.ptrStart)
    case _ => false
  }
  /** Hashes the contents of the map to an Int value.
    * 
    * By xor'ing all the map's keys and values together, we can be sure
    * that maps with the same contents will have the same hashCode
    * regardless of the order those items appear.
    */
  override def hashCode: Int = Map.hash(lhs)(ptrStart, 0xDEADD065)

  /** Returns a string representation of the contents of the map.
    */
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("Map(")
    @tailrec def rec(p: Ptr[Tag], prefix: String): Unit = p match {
      case VPtr(vp) =>
        sb.append(prefix)
        sb.append(ptrKey(vp).toString)
        sb.append(" -> ")
        sb.append(ptrValue(vp).toString)
        rec(ptrNext(vp), ", ")
      case _ =>
    }
    rec(ptrStart, "")
    sb.append(")")
    sb.toString
  }

}

trait MMap[K, V] extends Map[K, V] with AddKeys[K] with Removable[K] with Updatable[V] { lhs =>

  def copy: MMap[K, V]

}

object Map {

  @inline @tailrec final def isSubset[K, V](lm: Map[K, V], rm: Map[K, V])(lp: Ptr[lm.Tag]): Boolean = lp.asInstanceOf[Ptr[lm.Tag]] match {
    case VPtr(lvp) =>
      val k = lm.ptrKey(lvp)
      rm.ptrFind(k) match {
        case VPtr(rvp) if lm.ptrValue(lvp) == rm.ptrValue(rvp) => isSubset(lm, rm)(lm.ptrNext(lvp))
        case _ => false
      }
    case _ => true
  }

  @inline @tailrec final def hash[K, V](m: Map[K, V])(p: Ptr[m.Tag], h: Int): Int = p.asInstanceOf[Ptr[m.Tag]] match {
      case VPtr(vp) =>
        val k = m.ptrKey(vp)
        val v = m.ptrValue(vp)
        hash(m)(m.ptrNext(vp), h ^ k.## ^ v.##)
      case _ => h
  }

}
