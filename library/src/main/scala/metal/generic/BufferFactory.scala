package metal
package generic

import scala.reflect.ClassTag

trait BufferFactory {

  def empty[@specialized V:MetalTag:ClassTag]: generic.Buffer[V]

  def apply[@specialized V:MetalTag:ClassTag](items: V*): generic.Buffer[V]

  def fromArray[@specialized V:MetalTag:ClassTag](array: Array[V]): generic.Buffer[V]

  def fromIterable[@specialized V:MetalTag:ClassTag](items: Iterable[V]): generic.Buffer[V]

}
