package metal.immutable

import metal.Methods

final class HashMap2[K, V1, V2](
  private[metal] val keys: Array[K],
  private[metal] val buckets: Array[Byte],
  private[metal] val values1: Array[V1],
  private[metal] val values2: Array[V2],
  val size: Int,
  val used: Int,
  val mask: Int,
  val limit: Int)(implicit val K: Methods[K], val V1: Methods[V1], val V2: Methods[V2]) extends metal.HashMap2[K, V1, V2] with metal.immutable.Map2[K, V1, V2]
