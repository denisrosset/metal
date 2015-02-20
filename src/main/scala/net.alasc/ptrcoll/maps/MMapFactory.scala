package net.alasc.ptrcoll
package maps

import scala.{specialized => sp}
import scala.reflect.ClassTag

trait MMapFactory[KLB, VLB, KExtra[_]] {
  type KLBEv[K] = K <:< KLB
  def empty[@sp(Int, Long) K: ClassTag: KExtra: KLBEv, V: ClassTag]: MMap[K, V]
  def ofSize[@sp(Int, Long) K: ClassTag: KExtra: KLBEv, V: ClassTag](n: Int): MMap[K, V]
}
