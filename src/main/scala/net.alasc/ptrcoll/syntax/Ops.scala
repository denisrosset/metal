package net.alasc.ptrcoll
package syntax

import scala.{specialized => sp}

import machinist.{DefaultOps => Ops}

class HasPtrOps[P](val lhs: P) extends AnyVal  {
  def nextPtr(implicit ev: HasPtr[P]): P = macro Ops.unopWithEv[HasPtr[P], P]
  def hasAt(implicit ev: HasPtr[P]): Boolean = macro Ops.unopWithEv[HasPtr[P], P]
  def at[K](implicit ev: HasPtrAt[K, P]): K = macro Ops.unopWithEv[HasPtrAt[K, P], K]
  def atVal[V](implicit ev: HasPtrVal[V, P]): V = macro Ops.unopWithEv[HasPtrVal[V, P], V]
  def atVal1[V1](implicit ev: HasPtrVal1[V1, P]): V1 = macro Ops.unopWithEv[HasPtrVal1[V1, P], V1]
  def atVal2[V2](implicit ev: HasPtrVal2[V2, P]): V2 = macro Ops.unopWithEv[HasPtrVal2[V2, P], V2]
}
