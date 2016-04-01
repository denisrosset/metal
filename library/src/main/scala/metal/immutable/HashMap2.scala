package metal
package immutable

final class HashMap2[K, V1, V2](
  private[metal] val keys: Array[K],
  private[metal] val buckets: Array[Byte],
  private[metal] val values1: Array[V1],
  private[metal] val values2: Array[V2],
  val size: Int,
  val used: Int,
  val mask: Int,
  val limit: Int)(implicit val K: MetalTag[K], val V1: MetalTag[V1], val V2: MetalTag[V2])
    extends generic.HashMap2[K, V1, V2]
    with immutable.Map2[K, V1, V2]
