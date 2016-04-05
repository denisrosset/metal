package metal
package generic

import scala.reflect.ClassTag

trait BufferFactory {

  def empty[@specialized V:ClassTag]: generic.Buffer[V]

  def apply[@specialized V:ClassTag](items: V*): generic.Buffer[V]

  def fromArray[@specialized V:ClassTag](array: Array[V]): generic.Buffer[V]

  def fromIterable[@specialized V:ClassTag](items: Iterable[V]): generic.Buffer[V]

}
