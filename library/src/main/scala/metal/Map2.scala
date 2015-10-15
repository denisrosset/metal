package metal

import scala.reflect.ClassTag
import scala.annotation.tailrec

import spire.algebra.Order
import spire.util.Opt

trait IMap2[K, V1, V2] extends ShapeKV1V2 with Searchable[K] with Countable with Values1[V1] with Values2[V2] { lhs =>

  implicit def ctK: ClassTag[K]

  implicit def ctV1: ClassTag[V1]

  implicit def ctV2: ClassTag[V2]

  def copy: IMap2[K, V1, V2]

  /** Checks if two Map2 are equal.
    * 
    * Equal means the maps have the same types (which is checked
    * using the ClassTag instances) and the same contents.
    * 
    * Comparing Maps with any other collection types (including Scala's)
    *  will return false.
    */
  override def equals(rhs: Any): Boolean = rhs match {
    case rhs: IMap2[K, V1, V2] if lhs.size == rhs.size && lhs.ctK == rhs.ctK && lhs.ctV1 == rhs.ctV1 && lhs.ctV2 == rhs.ctV2 => IMap2.isSubset(lhs, rhs)(lhs.ptrStart)
    case _ => false
  }

  /** Hashes the contents of the map to an Int value.
    * 
    * By xor'ing all the map's keys and values together, we can be sure
    * that maps with the same contents will have the same hashCode
    * regardless of the order those items appear.
    */
  override def hashCode: Int = IMap2.hash(lhs)(ptrStart, 0xDEADD065)

  /** Returns a string representation of the contents of the map.
    */
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("Map(")
    @tailrec def rec(p: Ptr[Tag], prefix: String): Unit = p match {
      case VPtr(vp) =>
        sb.append(prefix)
        sb.append(ptrKey[K](vp).toString)
      sb.append(" -> (")
      sb.append(ptrValue1[V1](vp).toString)
      sb.append(", ")
      sb.append(ptrValue2[V2](vp).toString)
        sb.append(")")
        rec(ptrNext(vp), ", ")
      case _ =>
    }
    rec(ptrStart, "")
    sb.append(")")
    sb.toString
  }

}

trait Map2[K, V1, V2] extends IMap2[K, V1, V2] with AddKeys[K] with Removable[K] with Updatable1[V1] with Updatable2[V2] { lhs =>

  def copy: Map2[K, V1, V2]

}

object IMap2 {

  @inline @tailrec final def isSubset[K, V1, V2](lm: IMap2[K, V1, V2], rm: IMap2[K, V1, V2])(lp: Ptr[lm.Tag]): Boolean = lp.asInstanceOf[Ptr[lm.Tag]] match {
    case VPtr(lvp) =>
      val k = lm.ptrKey(lvp)
      rm.ptrFind(k) match {
        case VPtr(rvp) if lm.ptrValue1(lvp) == rm.ptrValue1(rvp) && lm.ptrValue2(lvp) == rm.ptrValue2(rvp) => isSubset(lm, rm)(lm.ptrNext(lvp))
        case _ => false
      }
    case _ => true
  }

  @inline @tailrec final def hash[K, V1, V2](m: IMap2[K, V1, V2])(p: Ptr[m.Tag], h: Int): Int = p.asInstanceOf[Ptr[m.Tag]] match {
    case VPtr(vp) =>
      val k = m.ptrKey(vp)
      val v1 = m.ptrValue1(vp)
      val v2 = m.ptrValue2(vp)
      hash(m)(m.ptrNext(vp), h ^ k.## ^ (v1.## * 41 + v2.##))
    case _ => h
  }

}
