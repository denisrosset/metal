package metal
package immutable

import scala.reflect.ClassTag
import generic.Methods

final class Buffer[@specialized V](private[metal] val array: Array[V], val length: Int)(implicit val V: Methods[V], val ct: ClassTag[V]) extends generic.Buffer[V] with immutable.Collection
