package metal
package immutable

final class HashSet[K](
  private[metal] val keys: Array[K],
  private[metal] val buckets: Array[Byte],
  val size: Int,
  val used: Int,
  val mask: Int,
  val limit: Int)(implicit val K: MetalTag[K]) extends generic.HashSet[K] with immutable.Set[K]
