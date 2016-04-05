package metal
package immutable

import scala.reflect.ClassTag

final class HashSet[K](
  private[metal] val keys: Array[K],
  private[metal] val buckets: Array[Byte],
  val size: Int,
  val used: Int,
  val mask: Int,
  val limit: Int
)(implicit
  val ctK: ClassTag[K],
  val K: MetalTag[K]
) extends generic.HashSet[K] with immutable.Set[K]
