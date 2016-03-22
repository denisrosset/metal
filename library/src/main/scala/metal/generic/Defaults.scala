package metal
package generic

import scala.annotation.tailrec
import spire.util.Opt

abstract class Defaults extends Collection { lhs =>

  type Generic >: lhs.type <: Defaults
  type Mutable <: metal.mutable.Collection with Defaults
  type Immutable <: metal.immutable.Collection with Defaults

  /** Returns the [[toString]] representation of the pointed element. */
  def ptrToString(p: VPtr[lhs.type]): String

  /** Returns the [[hashCode]] of the pointed element. */
  def ptrHash(p: VPtr[lhs.type]): Int

  /** Tests if the parameter has the same generic type, and returns an optional cast. */
  def ptrCastT(any: Any): Opt[Generic]

  /** Checks that the pointed element exists in the other collection and has the same value. */
  def ptrEquals(thisPtr: VPtr[lhs.type], that: Generic): Boolean

  /** Flag indicating that the [[equals]] methods of this object should be called in priority. */
  def priorityEquals: Boolean = false

  /** Checks if two collections are equal.
    * 
    * Equal means the collections have the same generic type (which is checked
    * by the method `ptrCastT`) and the same contents.
    * 
    * Comparing metal collections with any other collection types (including those from Scala)
    * will return false.
    */
  override def equals(any: Any): Boolean = ptrCastT(any) match {
    case Opt(rhs) =>
      if (!lhs.priorityEquals && rhs.priorityEquals) (rhs == lhs)
      else if (lhs.longSize == rhs.longSize) {
        @tailrec def rec(ptr: Ptr[lhs.type]): Boolean = ptr match {
          case IsVPtr(vp) =>
            if (ptrEquals(vp, rhs))
              rec(ptrNext(vp))
            else
              false
          case _ => true
        }
        rec(ptr)
      } else false
    case _ => false
  }

  /** Hashes the contents of the collection to an Int value.
    * 
    * By xor'ing all the collection's element hashes together, we can be sure
    * that collections with the same contents will have the same hashCode
    * regardless of the order those items appear.
    */
  override def hashCode: Int = {
    @tailrec def rec(p: Ptr[lhs.type], h: Int): Int = p match {
      case IsVPtr(vp) =>
        rec(ptrNext(vp), h ^ ptrHash(vp))
      case _ => h
    }
    rec(ptr, 0xDEADD065)
  }

  override def toString: String = {
    val sb = new StringBuilder
    sb.append(stringPrefix)
    sb.append("(")
    @tailrec def rec(p: Ptr[lhs.type], prefix: String): Unit = p match {
      case IsVPtr(vp) =>
        sb.append(prefix)
        sb.append(ptrToString(vp))
        rec(ptrNext(vp), ", ")
      case _ =>
    }
    rec(ptr, "")
    sb.append(")")
    sb.toString
  }

}
