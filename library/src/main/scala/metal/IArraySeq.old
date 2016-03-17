package metal

import scala.reflect.ClassTag

import spire.math.QuickSort

final class IArraySeq[@specialized V](val array: Array[V], val length: Long)(implicit val V: Methods[V]) extends ISeq[V] {

  def this(array: Array[V])(implicit V: Methods[V]) = this(array, array.length)

  type MType = Buffer[V]
  type IType = IArraySeq[V]

  override def stringPrefix: String = "IArraySeq"

  def apply(idx: Long): V = array(idx.toInt)

  def mutableCopy = new Buffer[V](array.clone, array.length)

  def toArray(implicit ct: ClassTag[V]): Array[V] = {
    val res = ct.newArray(length.toInt)
    Array.copy(array, 0, res, 0, length.toInt)
    res
  }

}
