package net.alasc.ptrcoll

import scala.language.experimental.macros

import scala.{specialized => spec}

import machinist.DefaultOps

class HasPtrOps[P](val lhs: P) extends AnyVal  {
  def next(implicit ev: GenHasPtr[P]): P = macro DefaultOps.unopWithEv[GenHasPtr[P], P]
  def hasAt(implicit ev: GenHasPtr[P]): Boolean = macro DefaultOps.unopWithEv[GenHasPtr[P], P]
  def at[A](implicit ev: HasPtr[A, P]): A = macro DefaultOps.unopWithEv[HasPtr[A, P], A]
}

object Syntax {
  implicit def toPtrOps[P](lhs: P): HasPtrOps[P] = new HasPtrOps(lhs)
}
