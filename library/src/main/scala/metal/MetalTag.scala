package metal

import scala.reflect.ClassTag

/** Type class enabling fast operations on primitive arrays. */
trait MetalTag[@specialized A] {

  /** Corresponding [[ClassTag]] */
  def classTag: ClassTag[A]

  /** Creates an array of length `n`. */
  def newArray(n: Int): Array[A] = classTag.newArray(n)

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

  override def equals(any: Any): Boolean = any match {
    case that: MetalTag[_] => MetalTag.this.classTag == that.classTag
    case _ => false
  }

  override def hashCode: Int = classTag.hashCode

}

object MetalTag {

  def apply[@specialized A](implicit A: MetalTag[A]): MetalTag[A] = A

  def make[A](implicit A: ClassTag[A]): MetalTag[A] = new MetalTag[A] {

    final def equalsElement(x: Array[A], ix: Int, y: Array[A], iy: Int): Boolean = x(ix) == y(iy)
    final def classTag: ClassTag[A] = A
    final def fillValue: A = classTag.newArray(0)(0)
    final def hash(a: A): Int = a.hashCode
    final def hashElement(a: Array[A], i: Int): Int = hash(a(i))

  }

  val Byte: MetalTag[Byte] = new MetalTag[Byte] {
    final def classTag = ClassTag.Byte
    final def fillValue = 0
    final def hash(a: Byte): Int = a.toInt
    final def hashElement(a: Array[Byte], i: Int): Int = hash(a(i))
    final def equalsElement(x: Array[Byte], ix: Int, y: Array[Byte], iy: Int) =
      x(ix) == y(iy)
  }

  val Short: MetalTag[Short] = new MetalTag[Short] {
    final def classTag = ClassTag.Short
    final def fillValue = 0
    final def hash(a: Short): Int = a.toInt
    final def hashElement(a: Array[Short], i: Int): Int = hash(a(i))
    final def equalsElement(x: Array[Short], ix: Int, y: Array[Short], iy: Int) =
      x(ix) == y(iy)
  }

  val Int: MetalTag[Int] = new MetalTag[Int] {
    final def classTag = ClassTag.Int
    final def fillValue = 0
    final def hash(a: Int): Int = a
    final def hashElement(a: Array[Int], i: Int): Int = hash(a(i))
    final def equalsElement(x: Array[Int], ix: Int, y: Array[Int], iy: Int) =
      x(ix) == y(iy)
  }

  val Long: MetalTag[Long] = new MetalTag[Long] {
    final def classTag = ClassTag.Long
    final def fillValue = 0L
    final def hash(a: Long): Int = (a ^ (a >>> 32)).toInt
    final def hashElement(a: Array[Long], i: Int): Int = hash(a(i))
    final def equalsElement(x: Array[Long], ix: Int, y: Array[Long], iy: Int) =
      x(ix) == y(iy)
  }

  val Boolean: MetalTag[Boolean] = new MetalTag[Boolean] {
    final def classTag = ClassTag.Boolean
    final def fillValue = false
    private[this] val trueHash =  true.hashCode
    private[this] val falseHash = false.hashCode
    final def hash(a: Boolean): Int = if (a) trueHash else falseHash
    final def hashElement(a: Array[Boolean], i: Int): Int = hash(a(i))
    final def equalsElement(x: Array[Boolean], ix: Int, y: Array[Boolean], iy: Int) =
      x(ix) == y(iy)
  }

  val Unit: MetalTag[Unit] = new MetalTag[Unit] {
    final def classTag = ClassTag.Unit
    final def fillValue = ()
    private[this] val unitHash =  ().hashCode
    final def hash(a: Unit): Int = unitHash
    final def hashElement(a: Array[Unit], i: Int): Int = unitHash
    final def equalsElement(x: Array[Unit], ix: Int, y: Array[Unit], iy: Int) = true
  }

  val Char: MetalTag[Char] = new MetalTag[Char] {
    final def classTag = ClassTag.Char
    final def fillValue = 0
    final def hash(a: Char): Int = java.lang.Character.hashCode(a)
    final def hashElement(a: Array[Char], i: Int): Int = hash(a(i))
    final def equalsElement(x: Array[Char], ix: Int, y: Array[Char], iy: Int) =
      x(ix) == y(iy)
  }


  val Float: MetalTag[Float] = new MetalTag[Float] {
    final def classTag = ClassTag.Float
    final def fillValue = 0.0f
    final def hash(a: Float): Int = java.lang.Float.floatToIntBits(a)
    final def hashElement(a: Array[Float], i: Int): Int = hash(a(i))
    final def equalsElement(x: Array[Float], ix: Int, y: Array[Float], iy: Int) =
      x(ix) == y(iy)
  }

  val Double: MetalTag[Double] = new MetalTag[Double] {
    final def classTag = ClassTag.Double
    final def fillValue = 0.0d
    final def hash(a: Double): Int = {
      val v = java.lang.Double.doubleToLongBits(a)
        ((v >>> 32) ^ v).toInt
    }
    final def hashElement(a: Array[Double], i: Int): Int = hash(a(i))
    final def equalsElement(x: Array[Double], ix: Int, y: Array[Double], iy: Int) =
      x(ix) == y(iy)
  }

  implicit def getMetalTag[@specialized A](implicit A: ClassTag[A]): MetalTag[A] = A match {
    case ClassTag.Byte => Byte.asInstanceOf[MetalTag[A]]
    case ClassTag.Short => Short.asInstanceOf[MetalTag[A]]
    case ClassTag.Int => Int.asInstanceOf[MetalTag[A]]
    case ClassTag.Long => Long.asInstanceOf[MetalTag[A]]
    case ClassTag.Boolean => Boolean.asInstanceOf[MetalTag[A]]
    case ClassTag.Unit => Unit.asInstanceOf[MetalTag[A]]
    case ClassTag.Char => Char.asInstanceOf[MetalTag[A]]
    case ClassTag.Float => Float.asInstanceOf[MetalTag[A]]
    case ClassTag.Double => Double.asInstanceOf[MetalTag[A]]
    case _ => make[A]
  }

}
