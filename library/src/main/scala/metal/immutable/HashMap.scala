package metal.immutable

import metal.Methods

final class HashMap[K, V](
  private[metal] val keys: Array[K],
  private[metal] val buckets: Array[Byte],
  private[metal] val values : Array[V],
  val size: Int,
  val used: Int,
  val mask: Int,
  val limit: Int)(implicit val K: Methods[K], val V: Methods[V]) extends metal.HashMap[K, V] with metal.immutable.Map[K, V]
