package metal

import scala.reflect.ClassTag

import spire.algebra.Order
import spire.math.QuickSort
import spire.syntax.all._

import impl._

final class Buffer[@specialized V](var array: Array[V], var length: Long)(implicit val V: Methods[V]) extends MSeq[V] {

  type MType = Buffer[V]
  type IType = IArraySeq[V]

  @inline final def apply(idx: Long): V = array(idx.toInt)

  def mutableCopy: Buffer[V] = new Buffer(array.clone, length)

  def result(): IArraySeq[V] = new IArraySeq[V](array, length)

  def sort()(implicit order: Order[V]): Unit = {
    QuickSort.qsort(array, 0, length.toInt - 1)(order, V.classTag)
  }

  def toArray: Array[V] = {
    val res = V.newArray(length.toInt)
    Array.copy(array, 0, res, 0, length.toInt)
    res
  }

  def clear(): Unit = {
    array = V.newArray(16)
    length = 0
  }

  override def stringPrefix = "Buffer"

  def +=(elem: V): this.type = {
    ensureLength(length + 1)
    array(length.toInt) = elem
    length += 1
    this
  }

  def update(idx: Long, v: V): Unit = {
    array(idx.toInt) = v
  }

  /** Grow if necessary the underlying array to accomodate at least n elements. */
  def ensureLength(n: Long): Dummy[V] = {
    val arrayLength: Long = array.length
    if (n > arrayLength) {
      var newLength: Long = arrayLength.toLong * 2
      while (n > newLength) newLength = newLength * 2
      if (newLength > Int.MaxValue) newLength = Int.MaxValue
      val newArray = V.newArray(newLength.toInt)
      scala.compat.Platform.arraycopy(array, 0, newArray, 0, length.toInt)
      array = newArray
    }
    null
  }

  def remove(idx: Long): V = {
    val last = length.toInt - 1
    if (idx < 0) throw new IndexOutOfBoundsException(idx.toString)
    else if (idx < last) {
      val v = array(idx.toInt)
      System.arraycopy(array, idx.toInt + 1, array, idx.toInt, last - idx.toInt)
      array(last) = null.asInstanceOf[V]
      length = last
      v
    } else if (idx == last) {
      val v = array(idx.toInt)
      array(last) = null.asInstanceOf[V]
      length = last
      v
    } else throw new IndexOutOfBoundsException(idx.toString)
  }

}

object Buffer {

  def empty[@specialized V:Methods]: Buffer[V] = new Buffer[V](Methods[V].newArray(16), 0)

  def apply[@specialized V:Methods](items: V*): Buffer[V] = {
    val array = Methods[V].newArray(items.size)
    val it = items.iterator
    var i = 0
    while (it.hasNext) {
      array(i) = it.next
      i += 1
    }
    new Buffer[V](array, array.length)

  }

}
