package metal
package immutable

import scala.reflect.ClassTag

final class HashMap[K, V](
  private[metal] val keys: Array[K],
  private[metal] val buckets: Array[Byte],
  private[metal] val values : Array[V],
  val size: Int,
  val used: Int,
  val mask: Int,
  val limit: Int
)(implicit
  val ctK: ClassTag[K],
  val K: MetalTag[K],
  val ctV: ClassTag[V],
  val V: MetalTag[V]
) extends generic.HashMap[K, V] with immutable.Map[K, V]
