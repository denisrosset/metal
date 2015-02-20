package net.alasc.ptrcoll
package syntax

import scala.{specialized => sp}

import machinist.{DefaultOps => Ops}

class HasPtrOps[P](val lhs: P) extends AnyVal  {
  def next(implicit ev: HasPtr[P]): P = macro Ops.unopWithEv[HasPtr[P], P]
  def hasAt(implicit ev: HasPtr[P]): Boolean = macro Ops.unopWithEv[HasPtr[P], P]
  def at[A](implicit ev: HasPtrAt[A, P]): A = macro Ops.unopWithEv[HasPtrAt[A, P], A]
  def atVal[V](implicit ev: HasPtrVal[V, P]): V = macro Ops.unopWithEv[HasPtrVal[V, P], V]
}
