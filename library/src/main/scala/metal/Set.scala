package metal

import scala.reflect.ClassTag
import scala.annotation.tailrec

import spire.algebra.Order

/** Immutable set part. */
trait ISet[K] extends ShapeK with Countable with Searchable[K] { lhs =>
  implicit def ctK: ClassTag[K]

  def copy: ISet[K]

  override def toString: String = {
    val c = lhs
    val sb = new StringBuilder
    sb.append("Set(")
    @tailrec def rec(p: Ptr[c.Tag], prefix: String): Unit = p match {
      case VPtr(vp) =>
        sb.append(prefix)
        sb.append(c.ptrKey(vp).toString)
        rec(c.ptrNext(vp), ", ")
      case _ =>
    }
    rec(c.ptrStart, "")
    sb.append(")")
    sb.toString
  }

  /**
    * Check if two SSets are equal.
    *
    * Equal means the sets have the same type (which is checked
    * using the ClassTag instances) and the same contents.
    *
    * Comparing SSets with any of Scala's collection types will
    * return false.
    */
  override def equals(rhs: Any): Boolean = rhs match {
    case rhs: ISet[K] if size == rhs.size && ctK == rhs.ctK =>
      val c = lhs.asInstanceOf[ISet[K]]
      val r = rhs.asInstanceOf[ISet[K]]
      @tailrec def rec(lp: Ptr[c.Tag]): Boolean = lp match {
        case VPtr(vlp) =>
          if (r.ptrFind[K](vlp.key).nonNull)
            rec(vlp.next)
          else
            false
        case _ => true
      }
      rec(c.ptrStart)
    case _ => false
  }

  /**
    * Hashes the contents of the set to an Int value.
    *
    * By xor'ing all the set's values together, we can be sure that
    * sets with the same contents will have the same hashCode
    * regardless of the order those elements appear.
    *
    * This is an O(n) operation.
    */
  override def hashCode: Int = {
    val c = lhs
    @tailrec def rec(p: Ptr[c.Tag], h: Int): Int = p match {
      case VPtr(vp) => rec(c.ptrNext(vp), h ^ c.ptrKey(vp).##)
      case _ => h
    }
    rec(c.ptrStart, 0xDEADD065)
  }
}

trait Set[K] extends ISet[K] with Removable[K] with AddKeys[K] { lhs =>

  def copy: Set[K]

}
