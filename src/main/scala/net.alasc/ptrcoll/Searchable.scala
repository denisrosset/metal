package net.alasc.ptrcoll

import scala.{specialized => sp}

trait Searchable[@sp(Int, Long) K] extends PointableKeys[K] {
  /** Returns a pointer to the given item, if it exists, or null. */
  def ptrFind(key: K): Ptr

  /** Returns whether `key` is present in the collection. */
  def contains(key: K): Boolean
}
