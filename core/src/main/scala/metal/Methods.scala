package metal

import scala.reflect.ClassTag

trait Methods[@specialized A] {

  implicit def classTag: ClassTag[A]

  def fillValue: A

  def hash(a: A): Int

  def hashElement(a: Array[A], i: Int): Int = hash(a(i))

  def equalsElement(x: Array[A], ix: Int, y: Array[A], iy: Int): Boolean = x(ix) == y(iy)

  def toStringElement(a: Array[A], i: Int): String = a(i).toString

  override def equals(any: Any): Boolean = any match {
    case that: Methods[_] => Methods.this.classTag == that.classTag
    case _ => false
  }

}

abstract class SpecMethods[@specialized A](val fillValue: A)(implicit val classTag: ClassTag[A])  extends Methods[A] {

  protected def hashSpec[@specialized B](b: B): Int =
    SpecMethods.this.asInstanceOf[Methods[B]].hash(b)

  override def hashElement(a: Array[A], i: Int): Int = (a.asInstanceOf[Array[_]]) match {
    case _: Array[Long] => hashSpec[Long](a.asInstanceOf[Array[Long]](i))
    case _: Array[Int] => hashSpec[Int](a.asInstanceOf[Array[Int]](i))
    case _: Array[Short] => hashSpec[Short](a.asInstanceOf[Array[Short]](i))
    case _: Array[Byte] => hashSpec[Byte](a.asInstanceOf[Array[Byte]](i))
    case _: Array[Double] => hashSpec[Double](a.asInstanceOf[Array[Double]](i))
    case _: Array[Float] => hashSpec[Float](a.asInstanceOf[Array[Float]](i))
    case _: Array[Boolean] => hashSpec[Boolean](a.asInstanceOf[Array[Boolean]](i))
    case _: Array[Char] => hashSpec[Char](a.asInstanceOf[Array[Char]](i))
    case _: Array[Unit] => hashSpec[Unit](a.asInstanceOf[Array[Unit]](i))
    case _ => hash(a(i))
  }

  protected def equalsSpec[@specialized B](x: Array[B], ix: Int, y: Array[B], iy: Int) =
    x(ix) == y(ix)


  override def equalsElement(x: Array[A], ix: Int, y: Array[A], iy: Int): Boolean =
    (x.asInstanceOf[Array[_]]) match {
      case _: Array[Long] => (x.asInstanceOf[Array[Long]](ix)) == (y.asInstanceOf[Array[Long]](iy))
      case _: Array[Int] => (x.asInstanceOf[Array[Int]](ix)) == (y.asInstanceOf[Array[Int]](iy))
      case _: Array[Short] => (x.asInstanceOf[Array[Short]](ix)) == (y.asInstanceOf[Array[Short]](iy))
      case _: Array[Byte] => (x.asInstanceOf[Array[Byte]](ix)) == (y.asInstanceOf[Array[Byte]](iy))
      case _: Array[Double] => (x.asInstanceOf[Array[Double]](ix)) == (y.asInstanceOf[Array[Double]](iy))
      case _: Array[Float] => (x.asInstanceOf[Array[Float]](ix)) == (y.asInstanceOf[Array[Float]](iy))
      case _: Array[Boolean] => (x.asInstanceOf[Array[Boolean]](ix)) == (y.asInstanceOf[Array[Boolean]](iy))
      case _: Array[Char] => (x.asInstanceOf[Array[Char]](ix)) == (y.asInstanceOf[Array[Char]](iy))
      case _: Array[Unit] => true
      case _ => x(ix) == y(iy)
    }

}

trait Methods0 {

  implicit def anyVal[A <: AnyVal: ClassTag]: Methods[A] = new Methods[A] {
    def classTag = implicitly[ClassTag[A]]
    def fillValue: A = new Array[A](0)(0)
    final def hash(a: A): Int = a.hashCode
  }
  
  implicit val anyRef: Methods[AnyRef] = new Methods[AnyRef] {
    def classTag = implicitly[ClassTag[AnyRef]]
    final def fillValue: AnyRef = null
    final def hash(a: AnyRef): Int = a.hashCode
  }

}

trait Methods1 extends Methods0 {

  implicit def arrayMethods[A: ClassTag](implicit A: Methods[A]): Methods[Array[A]] = new ArrayMethods[A]

}

/** Methods for arrays treating them the same as immutable sequences. 
  * The double specialization is needed to avoid boxing when using ArrayMethods unspecialized. */
final class ArrayMethods[@specialized A: ClassTag](implicit A: Methods[A]) extends Methods[Array[A]] {

  def classTag = implicitly[ClassTag[A]].wrap

  def fillValue: Array[A] = null

  def hashSpec[@specialized B](array: Array[B])(implicit d: Dummy[B]): Int = {
    val B = A.asInstanceOf[Methods[B]]
    import scala.util.hashing.MurmurHash3._
    var h = arraySeed
    var i = 0
    while (i < array.length) {
      h = mix(h, B.hash(array(i)))
      i += 1
    }
    finalizeHash(h, array.length)
  }

  def hash(a: Array[A]): Int = (a.asInstanceOf[Array[_]]) match {
    case array: Array[Long] => hashSpec[Long](array.asInstanceOf[Array[Long]])
    case array: Array[Int] => hashSpec[Int](array.asInstanceOf[Array[Int]])
    case array: Array[Short] => hashSpec[Short](array.asInstanceOf[Array[Short]])
    case array: Array[Byte] => hashSpec[Byte](array.asInstanceOf[Array[Byte]])
    case array: Array[Double] => hashSpec[Double](array.asInstanceOf[Array[Double]])
    case array: Array[Float] => hashSpec[Float](array.asInstanceOf[Array[Float]])
    case array: Array[Boolean] => hashSpec[Boolean](array.asInstanceOf[Array[Boolean]])
    case array: Array[Char] => hashSpec[Char](array.asInstanceOf[Array[Char]])
    case array: Array[Unit] => hashSpec[Unit](array.asInstanceOf[Array[Unit]])
    case array: Array[A] => hashSpec[A](a)
  }

}

object Methods extends Methods1 {

  def apply[@specialized A](implicit A: Methods[A]): Methods[A] = A

  implicit val byte: Methods[Byte] = new SpecMethods[Byte](0) {
    final def hash(a: Byte): Int = a.toInt
  }

  implicit val short: Methods[Short] = new SpecMethods[Short](0) {
    final def hash(a: Short): Int = a.toInt
  }

  implicit val int: Methods[Int] = new SpecMethods[Int](0) {
    final def hash(a: Int): Int = a
  }

  implicit val long: Methods[Long] = new SpecMethods[Long](0) {
    final def hash(a: Long): Int = (a ^ (a >>> 32)).toInt
  }

  implicit val boolean: Methods[Boolean] = new SpecMethods[Boolean](false) {
    private[this] val trueMethods =  true.hashCode
    private[this] val falseMethods = false.hashCode
    final def hash(a: Boolean): Int = if (a) trueMethods else falseMethods
  }

  implicit val unit: Methods[Unit] = new SpecMethods[Unit]( () ) {
    private[this] val unitMethods =  ().hashCode
    final def hash(a: Unit): Int = unitMethods
  }

  implicit val char: Methods[Char] = new SpecMethods[Char](0) {
    final def hash(a: Char): Int = java.lang.Character.hashCode(a)
  }


  implicit val float: Methods[Float] = new SpecMethods[Float](0.0f) {
    final def hash(a: Float): Int = java.lang.Float.floatToIntBits(a)
  }

  implicit val double: Methods[Double] = new SpecMethods[Double](0.0d) {
    final def hash(a: Double): Int = {
      val v = java.lang.Double.doubleToLongBits(a)
        ((v >>> 32) ^ v).toInt
    }
  }

  implicit val longArray: Methods[Array[Long]] = new ArrayMethods[Long]
  implicit val intArray: Methods[Array[Int]] = new ArrayMethods[Int]
  implicit val shortArray: Methods[Array[Short]] = new ArrayMethods[Short]
  implicit val byteArray: Methods[Array[Byte]] = new ArrayMethods[Byte]
  implicit val charArray: Methods[Array[Char]] = new ArrayMethods[Char]
  implicit val booleanArray: Methods[Array[Boolean]] = new ArrayMethods[Boolean]
  implicit val doubleArray: Methods[Array[Double]] = new ArrayMethods[Double]
  implicit val floatArray: Methods[Array[Float]] = new ArrayMethods[Float]
  implicit val unitArray: Methods[Array[Unit]] = new ArrayMethods[Unit]
  implicit val anyRefArray: Methods[Array[AnyRef]] = new ArrayMethods[AnyRef]

}
