package net.alasc.ptrcoll
package syntax

trait HasPtrSyntax {
  implicit def hasPtrOps[P](lhs: P): HasPtrOps[P] = new HasPtrOps(lhs)
}

trait AllSyntax extends
    HasPtrSyntax
