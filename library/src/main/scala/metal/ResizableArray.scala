package metal

import scala.reflect.ClassTag

import spire.syntax.cfor._

trait ResizableArray[@specialized V] extends ShapeV with Keys[Int] with Countable with Values[V] { self =>

  implicit def ct: ClassTag[V]
  var array: Array[V]
  var size: Int

  override def equals(that: Any): Boolean = that match {
    case s: ResizableArray[_] =>
      if (length != s.length || ct != s.ct) return false
      val buffer = s.asInstanceOf[ResizableArray[V]]
      val limit = length
      cfor(0)(_ < limit, _ + 1) { i =>
        if (array(i) != buffer.array(i)) return false
      }
      true
    case _ =>
      false
  }

  /**
    * Hash the contents of the buffer to an Int value.
    */
  override def hashCode: Int = {
    var code: Int = 0xf457f00d
    val limit = length
    cfor(0)(_ < limit, _ + 1) { i => code = (code * 19) + array(i).## }
    code
  }

  def prefix = "ResizableArray"

  /**
    * Return a string representation of the contents of the buffer.
    */
  override def toString =
    if (length == 0) {
      s"$prefix()"
    } else {
      val limit = length
      val sb = new StringBuilder()
      sb.append(s"$prefix(")
      sb.append(apply(0))
      cfor(1)(_ < limit, _ + 1) { i =>
        sb.append(", ")
        sb.append(array(i))
      }
      sb.append(")")
      sb.toString
    }

  def copy: ResizableArray[V]

  def length = size

  def isEmpty = size == 0

  def nonEmpty = size > 0

  /**
    * Return the value at element i.
    * 
    * If the index exceeds the length, the result is undefined; an exception could be
    * thrown, but this is not guaranteed.
    */
  def apply(idx: Int): V = array(idx)

  def +=(elem: V): self.type = {
    ensureSize(size + 1)
    array(size) = elem
    size += 1
    self
  }

  def update(idx: Int, v: V): Unit = {
    array(idx) = v
  }

  def reduceToSize(newSize: Int): Unit = {
    require(newSize <= size)
    while (size > newSize) {
      size -= 1
      array(size) = null.asInstanceOf[V]
    }
  }

  /** Grow if necessary the underlying array to accomodate at least n elements. */
  def ensureSize(n: Int): Dummy[V] = {
    val arrayLength: Long = array.length
    if (n > arrayLength) {
      var newSize: Long = arrayLength.toLong * 2
      while (n > newSize) newSize = newSize * 2
      if (newSize > Int.MaxValue) newSize = Int.MaxValue
      val newArray = new Array[V](newSize.toInt)
      scala.compat.Platform.arraycopy(array, 0, newArray, 0, size)
      array = newArray
    }
    null
  }

  /** Swap two elements of this array. */
  protected def swap(a: Int, b: Int): Dummy[V] = {
    val h = array(a)
    array(a) = array(b)
    array(b) = h
    null
  }

  /** Move parts of the array. */
  protected def copy(m: Int, n: Int, len: Int) = {
    scala.compat.Platform.arraycopy(array, m, array, n, len)
  }

  def remove(idx: Int): V = {
    val last = size - 1
    if (idx < 0) throw new IndexOutOfBoundsException(idx.toString)
    else if (idx < last) {
      val v = array(idx)
      System.arraycopy(array, idx + 1, array, idx, last - idx)
      array(last) = null.asInstanceOf[V]
      size = last
      v
    } else if (idx == last) {
      val v = array(idx)
      array(last) = null.asInstanceOf[V]
      size = last
      v
    } else throw new IndexOutOfBoundsException(idx.toString)
  }

  final def ptrFind[@specialized L](key: L): Ptr[Tag] = {
    val keyI = key.asInstanceOf[Int]
    if (keyI >= 0 && keyI < size) Ptr[Tag](key.asInstanceOf[Int]) else Ptr.Null
  }

  final def ptrValue[@specialized W](vp: VPtr[Tag]): W = array.asInstanceOf[Array[W]](vp.v.toInt)

  final def ptrStart: Ptr[Tag] = if (isEmpty) Ptr.Null else VPtr[Tag](0)

  final def ptrNext(ptr: VPtr[Tag]): Ptr[Tag] = if (ptr.v == size - 1) Ptr.Null[Tag] else VPtr[Tag](ptr.v + 1)

  final def ptrKey[@specialized L](ptr: VPtr[Tag]): L = ptr.v.toInt.asInstanceOf[L]

}
