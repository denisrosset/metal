package metal

import scala.reflect.ClassTag

import spire.algebra._
import spire.math.QuickSort
import spire.syntax.all._

import impl._

final class Buffer[@specialized V](var array: Array[V], var size: Int)(implicit val ct: ClassTag[V]) extends ResizableArray[V] {

  def copy: Buffer[V] = new Buffer(array.clone, size)

}

object Buffer {

  def empty[@specialized V:ClassTag]: Buffer[V] = new Buffer[V](new Array[V](16), 0)

  def apply[@specialized V:ClassTag](items: V*): Buffer[V] = new Buffer[V](Array[V](items: _*), items.size)

}
