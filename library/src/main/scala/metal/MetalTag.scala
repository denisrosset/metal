package metal

import scala.reflect.ClassTag

/** Type class enabling fast operations on primitive arrays. */
trait MetalTag[@specialized A] {

  /** Default value for array initialization, equivalent to `null.asInstanceOf[A]`. */
  def fillValue: A

  /** Hash of the given value. */
  def hash(a: A): Int

  /** Hash of the element at index `i` in array `a`. */
  def hashElement(a: Array[A], i: Int): Int

  /** Checks whether `x(ix)` is equal to `y(iy)`. */
  def equalsElement(x: Array[A], ix: Int, y: Array[A], iy: Int): Boolean

  /** Returns the string representation of the element `a(i)`. */
  def toStringElement(a: Array[A], i: Int): String = a(i).toString

  override def equals(any: Any): Boolean = sys.error("Cannot compare MetalTags")

  override def hashCode: Int = sys.error("Cannot hash MetalTags")

}

object MetalTag {

  def apply[@specialized A](implicit A: MetalTag[A]): MetalTag[A] = A

  object Any extends MetalTag[Any] {

    final def equalsElement(x: Array[Any], ix: Int, y: Array[Any], iy: Int): Boolean = x(ix) == y(iy)
    final def fillValue: Any = null.asInstanceOf[Any]
    final def hash(a: Any): Int = a.hashCode
    final def hashElement(a: Array[Any], i: Int): Int = hash(a(i))

  }

  def make[A](implicit A: ClassTag[A]): MetalTag[A] = new MetalTag[A] {

    final def equalsElement(x: Array[A], ix: Int, y: Array[A], iy: Int): Boolean = x(ix) == y(iy)
    final def fillValue: A = null.asInstanceOf[A]
    final def hash(a: A): Int = a.hashCode
    final def hashElement(a: Array[A], i: Int): Int = hash(a(i))

  }

  object Byte extends MetalTag[Byte] {
    final def fillValue = 0
    final def hash(a: Byte): Int = a.toInt
    final def hashElement(a: Array[Byte], i: Int): Int = hash(a(i))
    final def equalsElement(x: Array[Byte], ix: Int, y: Array[Byte], iy: Int) =
      x(ix) == y(iy)
  }

  object Short extends MetalTag[Short] {
    final def fillValue = 0
    final def hash(a: Short): Int = a.toInt
    final def hashElement(a: Array[Short], i: Int): Int = hash(a(i))
    final def equalsElement(x: Array[Short], ix: Int, y: Array[Short], iy: Int) =
      x(ix) == y(iy)
  }

  object Int extends MetalTag[Int] {
    final def fillValue = 0
    final def hash(a: Int): Int = a
    final def hashElement(a: Array[Int], i: Int): Int = hash(a(i))
    final def equalsElement(x: Array[Int], ix: Int, y: Array[Int], iy: Int) =
      x(ix) == y(iy)
  }

  object Long extends MetalTag[Long] {
    final def fillValue = 0L
    final def hash(a: Long): Int = (a ^ (a >>> 32)).toInt
    final def hashElement(a: Array[Long], i: Int): Int = hash(a(i))
    final def equalsElement(x: Array[Long], ix: Int, y: Array[Long], iy: Int) =
      x(ix) == y(iy)
  }

  object Boolean extends MetalTag[Boolean] {
    final def fillValue = false
    private[this] val trueHash =  true.hashCode
    private[this] val falseHash = false.hashCode
    final def hash(a: Boolean): Int = if (a) trueHash else falseHash
    final def hashElement(a: Array[Boolean], i: Int): Int = hash(a(i))
    final def equalsElement(x: Array[Boolean], ix: Int, y: Array[Boolean], iy: Int) =
      x(ix) == y(iy)
  }

  object Unit extends MetalTag[Unit] {
    final def fillValue = ()
    private[this] val unitHash =  ().hashCode
    final def hash(a: Unit): Int = unitHash
    final def hashElement(a: Array[Unit], i: Int): Int = unitHash
    final def equalsElement(x: Array[Unit], ix: Int, y: Array[Unit], iy: Int) = true
  }

  object Char extends MetalTag[Char] {
    final def fillValue = 0
    final def hash(a: Char): Int = java.lang.Character.hashCode(a)
    final def hashElement(a: Array[Char], i: Int): Int = hash(a(i))
    final def equalsElement(x: Array[Char], ix: Int, y: Array[Char], iy: Int) =
      x(ix) == y(iy)
  }

  object Float extends MetalTag[Float] {
    final def fillValue = 0.0f
    final def hash(a: Float): Int = java.lang.Float.floatToIntBits(a)
    final def hashElement(a: Array[Float], i: Int): Int = hash(a(i))
    final def equalsElement(x: Array[Float], ix: Int, y: Array[Float], iy: Int) =
      x(ix) == y(iy)
  }

  object Double extends MetalTag[Double] {
    final def fillValue = 0.0d
    final def hash(a: Double): Int = {
      val v = java.lang.Double.doubleToLongBits(a)
        ((v >>> 32) ^ v).toInt
    }
    final def hashElement(a: Array[Double], i: Int): Int = hash(a(i))
    final def equalsElement(x: Array[Double], ix: Int, y: Array[Double], iy: Int) =
      x(ix) == y(iy)
  }

  implicit def anyValMetalTag[@specialized A](implicit A: ClassTag[A]): MetalTag[A] =
    A match {
      case ClassTag.Byte => Byte.asInstanceOf[MetalTag[A]]
      case ClassTag.Short => Short.asInstanceOf[MetalTag[A]]
      case ClassTag.Int => Int.asInstanceOf[MetalTag[A]]
      case ClassTag.Long => Long.asInstanceOf[MetalTag[A]]
      case ClassTag.Boolean => Boolean.asInstanceOf[MetalTag[A]]
      case ClassTag.Unit => Unit.asInstanceOf[MetalTag[A]]
      case ClassTag.Char => Char.asInstanceOf[MetalTag[A]]
      case ClassTag.Float => Float.asInstanceOf[MetalTag[A]]
      case ClassTag.Double => Double.asInstanceOf[MetalTag[A]]
      case _ => Any.asInstanceOf[MetalTag[A]]
    }

}
