package metal

trait Primitive[@scala.specialized A] {
  def hash(l: Long): Int
  def equal(x: Long, y: Long): Boolean
  def encode(a: A): Long
  def decode(l: Long): A
  def arrayEqual(a: Array[_], i: Int, y: Long): Boolean
  def arrayRead(a: Array[_], i: Int): Long
  def arrayWrite(a: Array[_], i: Int, l: Long): Unit
  def bytes: Int
}

object Primitive {

/*  implicit object double extends Primitive[Double] {
    @inline final def encode(a: Double): Long = java.lang.Double.doubleToRawLongBits(a)
    @inline final def decode(l: Long): Double = java.lang.Double.longBitsToDouble(l)
    @inline final def bytes = 8
  }

  implicit object float extends Primitive[Float] {
    @inline final def encode(a: Float): Long = java.lang.Float.floatToRawIntBits(a).toLong
    @inline final def decode(l: Long): Float = java.lang.Float.intBitsToFloat(l.toInt)
    @inline final def bytes = 4
  }*/

  implicit object long extends Primitive[Long] {
    @inline final def equal(x: Long, y: Long) = x == y
    @inline final def hash(x: Long): Int = (x ^ (x >>> 32)).toInt
    @inline final override def arrayEqual(a: Array[_], i: Int, y: Long): Boolean = a.asInstanceOf[Array[Long]](i) == y
    @inline final override def arrayRead(a: Array[_], i: Int): Long = a.asInstanceOf[Array[Long]](i)
    @inline final override def arrayWrite(a: Array[_], i: Int, l: Long): Unit = { a.asInstanceOf[Array[Long]](i) = l }
    @inline final def encode(a: Long): Long = a
    @inline final def decode(l: Long): Long = l
    @inline final def bytes = 8
  }

  implicit object int extends Primitive[Int] {
    @inline final def equal(x: Long, y: Long): Boolean = x == y
    @inline final def hash(x: Long): Int = x.toInt
    @inline final override def arrayEqual(a: Array[_], i: Int, y: Long): Boolean = a.asInstanceOf[Array[Int]](i) == y.toInt
    @inline final override def arrayRead(a: Array[_], i: Int): Long = a.asInstanceOf[Array[Int]](i).toLong
    @inline final override def arrayWrite(a: Array[_], i: Int, l: Long): Unit = { a.asInstanceOf[Array[Int]](i) = l.toInt }
    @inline final def encode(a: Int): Long = a.toLong
    @inline final def decode(l: Long): Int = l.toInt
    @inline final def bytes = 4
  }
/*
  implicit object short extends Primitive[Short] {
    @inline final def encode(a: Short): Long = a.toLong
    @inline final def decode(l: Long): Short = l.toShort
    @inline final def bytes = 2
  }

  implicit object byte extends Primitive[Byte] {
    @inline final def encode(a: Byte): Long = a.toLong
    @inline final def decode(l: Long): Byte = l.toByte
    @inline final def bytes = 1
  }*/

}
