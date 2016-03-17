package metal.immutable

import metal.Methods

final class HashSet[K](
  private[metal] val keys: Array[K],
  private[metal] val buckets: Array[Byte],
  val size: Int,
  val used: Int,
  val mask: Int,
  val limit: Int)(implicit val K: Methods[K]) extends metal.HashSet[K] with metal.immutable.Set[K] {

}
