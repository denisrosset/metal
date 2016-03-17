package metal.immutable

import scala.reflect.ClassTag
import metal.Methods

final class Buffer[@specialized V](private[metal] val array: Array[V], val length: Int)(implicit val V: Methods[V], val ct: ClassTag[V]) extends metal.Buffer[V] with metal.immutable.Collection
