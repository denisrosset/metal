package metal
package immutable

import scala.reflect.{classTag, ClassTag}

final class Buffer[@specialized V](private[metal] val array: Array[V], val length: Int)(implicit val V: MetalTag[V], val ct: ClassTag[V]) extends generic.Buffer[V] with immutable.Collection {

  @inline final def apply(idx: Int): V = array(idx)

}

object Buffer extends generic.BufferFactory {

  val startSize = 8

  def empty[@specialized V:MetalTag:ClassTag]: immutable.Buffer[V] = new immutable.Buffer[V](classTag[V].newArray(0), 0)

  def apply[@specialized V:MetalTag:ClassTag](items: V*): immutable.Buffer[V] = {
    val array = classTag[V].newArray(items.size)
    val it = items.iterator
    var i = 0
    while (it.hasNext) {
      array(i) = it.next
      i += 1
    }
    new immutable.Buffer[V](array, array.length)
  }

  def fromArray[@specialized V:MetalTag:ClassTag](array: Array[V]): immutable.Buffer[V] =
    new immutable.Buffer[V](array.clone, array.length)

  def fromIterable[@specialized V:MetalTag:ClassTag](iterable: Iterable[V]): immutable.Buffer[V] = {
    val array = classTag[V].newArray(iterable.size)
    val it = iterable.iterator
    var i = 0
    while (it.hasNext) {
      array(i) = it.next
      i += 1
    }
    new immutable.Buffer[V](array, array.length)
  }

}
