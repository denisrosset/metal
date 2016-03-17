package metal

import scala.reflect.ClassTag

import spire.algebra.Order
import spire.math.QuickSort
import spire.syntax.all._

/* We do not extract a common `Seq` base type, because [[Buffer]] would be its only subtype.
 * Let's make a `Seq` base trait when we have more input on its design.
 */
trait Buffer[@specialized V] extends Collection with NElements1[V] with Enumerable with Values[V] { self =>

  implicit def ct: ClassTag[V]
  implicit def V: Methods[V]

  type Mutable = metal.mutable.Buffer[V]
  type Immutable = metal.immutable.Buffer[V]

  private[metal] def array: Array[V]

  /**
    * Return the value at element i.
    * 
    * If the index exceeds the length, the result is undefined; an exception could be
    * thrown, but this is not guaranteed.
    */
  def apply(idx: Int): V = array(idx)

  def length: Int

  @inline final def longSize = length

  @inline final def isEmpty = length == 0

  @inline final def nonEmpty = length > 0

  def stringPrefix = "Buffer"

  final def ptr: Ptr[self.type] = if (isEmpty) Ptr.Null(self) else VPtr(self, 0)

  final def ptrNext(ptr: VPtr[self.type]): Ptr[self.type] =
    if (ptr.raw == length - 1) Ptr.Null(self) else VPtr(self, ptr.raw + 1)

  final def ptrElement1[@specialized E](ptr: VPtr[self.type]): E = this.asInstanceOf[Buffer[E]].apply(ptr.raw.toInt)

  final def ptrValue[@specialized W](ptr: VPtr[self.type]): W = this.asInstanceOf[Buffer[W]].apply(ptr.raw.toInt)

  final def toArray: Array[V] = {
    val res = ct.newArray(length.toInt)
    Array.copy(array, 0, res, 0, length)
    res
  }

  def mutableCopy = new metal.mutable.Buffer(array.clone, length)

  override def equals(that: Any): Boolean = that match {
    case s: Buffer[_] =>
      val len = length
      if (V != s.V) return false
      V match {
        case Methods.Long =>
          Buffer.specEquals[Long](self.asInstanceOf[Buffer[Long]], that.asInstanceOf[Buffer[Long]])
        case Methods.Int =>
          Buffer.specEquals[Int](self.asInstanceOf[Buffer[Int]], that.asInstanceOf[Buffer[Int]])
        case Methods.Short =>
          Buffer.specEquals[Short](self.asInstanceOf[Buffer[Short]], that.asInstanceOf[Buffer[Short]])
        case Methods.Byte =>
          Buffer.specEquals[Byte](self.asInstanceOf[Buffer[Byte]], that.asInstanceOf[Buffer[Byte]])
        case Methods.Char =>
          Buffer.specEquals[Char](self.asInstanceOf[Buffer[Char]], that.asInstanceOf[Buffer[Char]])
        case Methods.Boolean =>
          Buffer.specEquals[Boolean](self.asInstanceOf[Buffer[Boolean]], that.asInstanceOf[Buffer[Boolean]])
        case Methods.Double =>
          Buffer.specEquals[Double](self.asInstanceOf[Buffer[Double]], that.asInstanceOf[Buffer[Double]])
        case Methods.Float =>
          Buffer.specEquals[Float](self.asInstanceOf[Buffer[Float]], that.asInstanceOf[Buffer[Float]])
        case Methods.Unit =>
          Buffer.specEquals[Unit](self.asInstanceOf[Buffer[Unit]], that.asInstanceOf[Buffer[Unit]])
        case _ =>
          Buffer.specEquals[V](self.asInstanceOf[Buffer[V]], that.asInstanceOf[Buffer[V]])
      }
    case _ => false
  }

  override def hashCode: Int = V match {
    case Methods.Long => Buffer.specHashCode[Long](self.asInstanceOf[Buffer[Long]])
    case Methods.Int => Buffer.specHashCode[Int](self.asInstanceOf[Buffer[Int]])
    case Methods.Short => Buffer.specHashCode[Short](self.asInstanceOf[Buffer[Short]])
    case Methods.Byte => Buffer.specHashCode[Byte](self.asInstanceOf[Buffer[Byte]])
    case Methods.Char => Buffer.specHashCode[Char](self.asInstanceOf[Buffer[Char]])
    case Methods.Boolean => Buffer.specHashCode[Boolean](self.asInstanceOf[Buffer[Boolean]])
    case Methods.Double => Buffer.specHashCode[Double](self.asInstanceOf[Buffer[Double]])
    case Methods.Float => Buffer.specHashCode[Float](self.asInstanceOf[Buffer[Float]])
    case Methods.Unit => Buffer.specHashCode[Unit](self.asInstanceOf[Buffer[Unit]])
    case _ => Buffer.specHashCode[V](self)
  }

  override def toString: String = {
    val sb = new StringBuilder
    sb.append(stringPrefix)
    sb.append("(")
    var prefix = ""
    val len = self.length
    cforRange(0 until len) { i =>
      sb.append(prefix)
      sb.append(apply(i).toString)
      prefix = ", "
    }
    sb.append(")")
    sb.toString
  }

}

object Buffer {

  def specEquals[@specialized V](lhs: Buffer[V], rhs: Buffer[V]): Boolean = {
    val len = lhs.length
    if (len != rhs.length) return false
    cforRange(0 until len) { i =>
      if (lhs.apply(i) != rhs.apply(i)) return false
    }
    true
  }

  def specHashCode[@specialized V](lhs: Buffer[V]): Int = {
    import lhs.V
    import scala.util.hashing.MurmurHash3._
    val len = lhs.length
    var h = arraySeed
    cforRange(0 until len) { i =>
      h = mix(h, V.hash(lhs.apply(i)))
    }
    finalizeHash(h, len)
  }

}
