package metal

import scala.reflect.ClassTag

import spire.algebra._
import spire.math.QuickSort
import spire.syntax.all._

import impl._

final class IArraySeq[@specialized V](val array: Array[V], val length: Long)(implicit val V: Methods[V]) extends ISeq[V] {

  def this(array: Array[V])(implicit V: Methods[V]) = this(array, array.length)

  type MType = Buffer[V]
  type IType = IArraySeq[V]

  override def stringPrefix: String = "IArraySeq"

  def apply(idx: Long): V = array(idx.toInt)

  def mutableCopy() = new Buffer[V](array.clone, array.length)

}
