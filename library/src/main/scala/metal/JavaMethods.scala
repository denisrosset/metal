package metal

import scala.annotation.tailrec

import spire.util.Opt

trait JavaMethods[T <: JavaMethods[T]] extends Enumerable { lhs: T =>

  def ptrToString(p: MyVPtr): String

  def ptrHash(p: MyVPtr): Int

  def ptrCastT(any: Any): Opt[T]

  def ptrEquals(thisPtr: MyVPtr, that: T): Boolean

  def priorityEquals: Boolean = false

  /** Checks if two collections are equal.
    * 
    * Equal means the collections have the same types (which is checked
    * by the method `ptrCastT`) and the same contents.
    * 
    * Comparing metal collections with any other collection types (including Scala's)
    * will return false.
    */
  override def equals(any: Any): Boolean = ptrCastT(any) match {
    case Opt(rhs) =>
      if (!lhs.priorityEquals && rhs.priorityEquals) (rhs == lhs)
      else if (lhs.longSize == rhs.longSize) {
        @tailrec def rec(ptr: MyPtr): Boolean = ptr match {
          case IsVPtr(vp) if ptrEquals(vp, rhs) => rec(ptrNext(vp))
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
    @tailrec def rec(p: MyPtr, h: Int): Int = p match {
      case IsVPtr(vp) =>
        rec(ptrNext(vp), h ^ ptrHash(vp))
      case _ => h
    }
    rec(ptr, 0xDEADD065)
  }

  def stringPrefix = "Pointable"

  override def toString: String = {
    val sb = new StringBuilder
    sb.append(stringPrefix)
    sb.append("(")
    @tailrec def rec(p: MyPtr, prefix: String): Unit = p match {
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

