package metal
package immutable

final class HashMap[K, V](
  private[metal] val keys: Array[K],
  private[metal] val buckets: Array[Byte],
  private[metal] val values : Array[V],
  val size: Int,
  val used: Int,
  val mask: Int,
  val limit: Int)(implicit val K: MetalTag[K], val V: MetalTag[V]) extends generic.HashMap[K, V] with immutable.Map[K, V]
