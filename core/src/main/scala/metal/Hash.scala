package metal

import scala.reflect.ClassTag

trait Hash[@specialized A] {

  def hash(a: A): Int

  def hashElement(a: Array[A], i: Int): Int = hash(a(i))

}

trait SpecHash[@specialized A] extends Hash[A] {

  protected def hashSpec[@specialized B](b: B): Int =
    SpecHash.this.asInstanceOf[Hash[B]].hash(b)

  override def hashElement(a: Array[A], i: Int): Int = (a.asInstanceOf[Array[_]]) match {
    case array: Array[Long] => hashSpec[Long](array.asInstanceOf[Array[Long]](i))
    case array: Array[Int] => hashSpec[Int](array.asInstanceOf[Array[Int]](i))
    case array: Array[Short] => hashSpec[Short](array.asInstanceOf[Array[Short]](i))
    case array: Array[Byte] => hashSpec[Byte](array.asInstanceOf[Array[Byte]](i))
    case array: Array[Double] => hashSpec[Double](array.asInstanceOf[Array[Double]](i))
    case array: Array[Float] => hashSpec[Float](array.asInstanceOf[Array[Float]](i))
    case array: Array[Boolean] => hashSpec[Boolean](array.asInstanceOf[Array[Boolean]](i))
    case array: Array[Char] => hashSpec[Char](array.asInstanceOf[Array[Char]](i))
    case array: Array[Unit] => hashSpec[Unit](array.asInstanceOf[Array[Unit]](i))
    case _ => hash(a(i))
  }

}

trait Hash0 {

  implicit val anyRef: Hash[AnyRef] = new Hash[AnyRef] {
    final def hash(a: AnyRef): Int = a.hashCode
  }

}

trait Hash1 extends Hash0 {

  implicit def genericHash[A <: AnyVal] = new Hash[A] {
    def hash(a: A): Int = a.hashCode
  }

  implicit def arrayHash[A: ClassTag](implicit A: Hash[A]): Hash[Array[A]] = new ArrayHash[A]

}

/** Hash for arrays treating them the same as immutable sequences. 
  * The double specialization is needed to avoid boxing. */
final class ArrayHash[@specialized A: ClassTag](implicit A: Hash[A]) extends Hash[Array[A]] {

  def hashSpec[@specialized B](array: Array[B])(implicit d: Dummy[B]): Int = {
    val B = A.asInstanceOf[Hash[B]]
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

object Hash extends Hash1 {

  def apply[@specialized A](implicit A: Hash[A]): Hash[A] = A

  implicit val byte: Hash[Byte] = new SpecHash[Byte] {
    final def hash(a: Byte): Int = a.toInt
  }

  implicit val short: Hash[Short] = new SpecHash[Short] {
    final def hash(a: Short): Int = a.toInt
  }

  implicit val int: Hash[Int] = new SpecHash[Int] {
    final def hash(a: Int): Int = a
  }

  implicit val long: Hash[Long] = new SpecHash[Long] {
    final def hash(a: Long): Int = (a ^ (a >>> 32)).toInt
  }

  implicit val boolean: Hash[Boolean] = new SpecHash[Boolean] {
    private[this] val trueHash =  true.hashCode
    private[this] val falseHash = false.hashCode
    final def hash(a: Boolean): Int = if (a) trueHash else falseHash
  }

  implicit val unit: Hash[Unit] = new SpecHash[Unit] {
    private[this] val unitHash =  ().hashCode
    final def hash(a: Unit): Int = unitHash
  }

  implicit val char: Hash[Char] = new SpecHash[Char] {
    final def hash(a: Char): Int = java.lang.Character.hashCode(a)
  }


  implicit val float: Hash[Float] = new SpecHash[Float] {
    final def hash(a: Float): Int = java.lang.Float.floatToIntBits(a)
  }

  implicit val double: Hash[Double] = new SpecHash[Double] {
    final def hash(a: Double): Int = {
      val v = java.lang.Double.doubleToLongBits(a)
        ((v >>> 32) ^ v).toInt
    }
  }

  implicit val longArray: Hash[Array[Long]] = new ArrayHash[Long]
  implicit val intArray: Hash[Array[Int]] = new ArrayHash[Int]
  implicit val shortArray: Hash[Array[Short]] = new ArrayHash[Short]
  implicit val byteArray: Hash[Array[Byte]] = new ArrayHash[Byte]
  implicit val charArray: Hash[Array[Char]] = new ArrayHash[Char]
  implicit val booleanArray: Hash[Array[Boolean]] = new ArrayHash[Boolean]
  implicit val doubleArray: Hash[Array[Double]] = new ArrayHash[Double]
  implicit val floatArray: Hash[Array[Float]] = new ArrayHash[Float]
  implicit val unitArray: Hash[Array[Unit]] = new ArrayHash[Unit]
  implicit val anyRefArray: Hash[Array[AnyRef]] = new ArrayHash[AnyRef]


}
