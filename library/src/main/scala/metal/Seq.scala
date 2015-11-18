package metal

import scala.reflect.ClassTag

import spire.syntax.cfor._

object FSeq {

  def specEquals[@specialized V](lhs: FSeq[V], rhs: FSeq[V]): Boolean = {
    val len = lhs.length
    if (len != rhs.length) return false
    cfor(0L)(_ < len, _ + 1L) { i =>
      if (lhs.apply(i) != rhs.apply(i)) return false
    }
    true
  }

  def specHashCode[@specialized V](lhs: FSeq[V])(implicit V: Methods[V]): Int = {
    import scala.util.hashing.MurmurHash3._
    val len = lhs.length
    var h = arraySeed
    cfor(0L)(_ < len, _ + 1L) { i =>
      h = mix(h, V.hash(lhs.apply(i)))
    }
    finalizeHash(h, Methods.Long.hash(len))
  }

}


trait FSeq[@specialized V] extends FColl with Elements[V] with Enumerable with Values[V] { self =>

  implicit def V: Methods[V]

  type Cap <: Nextable with Values[V] with Elements[V]

  type IType <: ISeq[V]
  type MType <: MSeq[V]

  def mutableCopy(): MSeq[V] with MType

  def stringPrefix = "FSeq"

  def length: Long

  /**
    * Return the value at element i.
    * 
    * If the index exceeds the length, the result is undefined; an exception could be
    * thrown, but this is not guaranteed.
    */
  def apply(idx: Long): V

  def isEmpty = length == 0

  def nonEmpty = length > 0

  @inline final def longSize: Long = length

  override def equals(that: Any): Boolean = that match {
    case s: FSeq[_] =>
      val len = length
      if (V != s.V) return false
      V match {
        case Methods.Long =>
          FSeq.specEquals[Long](self.asInstanceOf[FSeq[Long]], that.asInstanceOf[FSeq[Long]])
        case Methods.Int =>
          FSeq.specEquals[Int](self.asInstanceOf[FSeq[Int]], that.asInstanceOf[FSeq[Int]])
        case Methods.Short =>
          FSeq.specEquals[Short](self.asInstanceOf[FSeq[Short]], that.asInstanceOf[FSeq[Short]])
        case Methods.Byte =>
          FSeq.specEquals[Byte](self.asInstanceOf[FSeq[Byte]], that.asInstanceOf[FSeq[Byte]])
        case Methods.Char =>
          FSeq.specEquals[Char](self.asInstanceOf[FSeq[Char]], that.asInstanceOf[FSeq[Char]])
        case Methods.Boolean =>
          FSeq.specEquals[Boolean](self.asInstanceOf[FSeq[Boolean]], that.asInstanceOf[FSeq[Boolean]])
        case Methods.Double =>
          FSeq.specEquals[Double](self.asInstanceOf[FSeq[Double]], that.asInstanceOf[FSeq[Double]])
        case Methods.Float =>
          FSeq.specEquals[Float](self.asInstanceOf[FSeq[Float]], that.asInstanceOf[FSeq[Float]])
        case Methods.Unit =>
          FSeq.specEquals[Unit](self.asInstanceOf[FSeq[Unit]], that.asInstanceOf[FSeq[Unit]])
        case _ =>
          FSeq.specEquals[V](self.asInstanceOf[FSeq[V]], that.asInstanceOf[FSeq[V]])
      }
    case _ => false
  }

  override def hashCode: Int = V match {
    case Methods.Long => FSeq.specHashCode[Long](self.asInstanceOf[FSeq[Long]])
    case Methods.Int => FSeq.specHashCode[Int](self.asInstanceOf[FSeq[Int]])
    case Methods.Short => FSeq.specHashCode[Short](self.asInstanceOf[FSeq[Short]])
    case Methods.Byte => FSeq.specHashCode[Byte](self.asInstanceOf[FSeq[Byte]])
    case Methods.Char => FSeq.specHashCode[Char](self.asInstanceOf[FSeq[Char]])
    case Methods.Boolean => FSeq.specHashCode[Boolean](self.asInstanceOf[FSeq[Boolean]])
    case Methods.Double => FSeq.specHashCode[Double](self.asInstanceOf[FSeq[Double]])
    case Methods.Float => FSeq.specHashCode[Float](self.asInstanceOf[FSeq[Float]])
    case Methods.Unit => FSeq.specHashCode[Unit](self.asInstanceOf[FSeq[Unit]])
    case _ => FSeq.specHashCode[V](self)
  }

  override def toString: String = {
    val sb = new StringBuilder
    sb.append(stringPrefix)
    sb.append("(")
    var prefix = ""
    val len = self.length
    cfor(0L)(_ < len, _ + 1L) { i =>
      sb.append(prefix)
      sb.append(apply(i).toString)
      prefix = ", "
    }
    sb.append(")")
    sb.toString
  }

  final def ptr: MyPtr = if (isEmpty) Ptr.`null`(this) else VPtr(this, 0)

  final def ptrNext(ptr: MyVPtr): MyPtr = if (ptr.raw == length - 1) Ptr.`null`(this) else VPtr(this, ptr.raw + 1)


  final def ptrElement[@specialized E](ptr: MyVPtr): E = this.asInstanceOf[FSeq[E]].apply(ptr.raw)

  final def ptrValue[@specialized W](ptr: MyVPtr): W = this.asInstanceOf[FSeq[W]].apply(ptr.raw)

}

trait ISeq[V] extends IColl with FSeq[V]

trait MSeq[V] extends MColl with FSeq[V] { self =>

  def result(): ISeq[V] with IType

  def +=(elem: V): self.type

  def update(idx: Long, v: V): Unit

  def remove(idx: Long): V

}
