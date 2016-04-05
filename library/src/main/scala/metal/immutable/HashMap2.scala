package metal
package immutable

import scala.reflect.ClassTag

final class HashMap2[K, V1, V2](
  private[metal] val keys: Array[K],
  private[metal] val buckets: Array[Byte],
  private[metal] val values1: Array[V1],
  private[metal] val values2: Array[V2],
  val size: Int,
  val used: Int,
  val mask: Int,
  val limit: Int
)(implicit
  val ctK: ClassTag[K],
  val K: MetalTag[K],
  val ctV1: ClassTag[V1],
  val V1: MetalTag[V1],
  val ctV2: ClassTag[V2],
  val V2: MetalTag[V2]
) extends generic.HashMap2[K, V1, V2]
    with immutable.Map2[K, V1, V2]
