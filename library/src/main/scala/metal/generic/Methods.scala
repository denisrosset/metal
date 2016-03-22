package metal
package generic

import scala.reflect.ClassTag

trait Methods[@specialized A] {

  protected def classTag: ClassTag[A]

  def newArray(n: Int): Array[A] = classTag.newArray(n)

  def fillValue: A

  def hash(a: A): Int

  def hashElement(a: Array[A], i: Int): Int = hash(a(i))

  def equalsElement(x: Array[A], ix: Int, y: Array[A], iy: Int): Boolean = x(ix) == y(iy)

  def toStringElement(a: Array[A], i: Int): String = a(i).toString

  override def equals(any: Any): Boolean = any match {
    case that: Methods[_] => Methods.this.classTag == that.classTag
    case _ => false
  }

  override def hashCode: Int = classTag.hashCode

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

abstract class Methods0 {

  implicit def fromClassTag[A](implicit A: ClassTag[A]): Methods[A] = new Methods[A] {

    protected def classTag: ClassTag[A] = A
    def fillValue: A = classTag.newArray(0)(0)
    final def hash(a: A): Int = a.hashCode

  }

}

abstract class Methods1 extends Methods0 {


  object AnyRefMethods extends Methods[AnyRef] {

    protected def classTag: ClassTag[AnyRef] = implicitly[ClassTag[AnyRef]]
    final def fillValue: AnyRef = null
    final def hash(a: AnyRef): Int = a.hashCode

  }

  implicit def anyRef[A <: AnyRef]: Methods[A] = AnyRefMethods.asInstanceOf[Methods[A]]

}

object Methods extends Methods1 {

  def apply[@specialized A](implicit A: Methods[A]): Methods[A] = A

  implicit val Byte: Methods[Byte] = new SpecMethods[Byte](0) {
    final def hash(a: Byte): Int = a.toInt
  }

  implicit val Short: Methods[Short] = new SpecMethods[Short](0) {
    final def hash(a: Short): Int = a.toInt
  }

  implicit val Int: Methods[Int] = new SpecMethods[Int](0) {
    final def hash(a: Int): Int = a
  }

  implicit val Long: Methods[Long] = new SpecMethods[Long](0) {
    final def hash(a: Long): Int = (a ^ (a >>> 32)).toInt
  }

  implicit val Boolean: Methods[Boolean] = new SpecMethods[Boolean](false) {
    private[this] val trueHash =  true.hashCode
    private[this] val falseHash = false.hashCode
    final def hash(a: Boolean): Int = if (a) trueHash else falseHash
  }

  implicit val Unit: Methods[Unit] = new SpecMethods[Unit]( () ) {
    private[this] val unitHash =  ().hashCode
    final def hash(a: Unit): Int = unitHash
  }

  implicit val Char: Methods[Char] = new SpecMethods[Char](0) {
    final def hash(a: Char): Int = java.lang.Character.hashCode(a)
  }


  implicit val Float: Methods[Float] = new SpecMethods[Float](0.0f) {
    final def hash(a: Float): Int = java.lang.Float.floatToIntBits(a)
  }

  implicit val Double: Methods[Double] = new SpecMethods[Double](0.0d) {
    final def hash(a: Double): Int = {
      val v = java.lang.Double.doubleToLongBits(a)
        ((v >>> 32) ^ v).toInt
    }
  }

}
