package metal

import scala.annotation.tailrec
import spire.util.Opt

abstract class Collection extends Enumerable { lhs =>

  /** Generic type of this collection (e.g. Map, Map2, Set). */
  type Generic >: lhs.type <: Collection

  /** Mutable variant of this collection. */
  type Mutable <: metal.mutable.Collection

  /** Immutable variant of this collection. */
  type Immutable <: metal.immutable.Collection

  /** Creates a mutable copy of this collection. */
  def mutableCopy: Mutable

  /** Returns this collection if already immutable, or an immutable copy. */
  def toImmutable: Immutable

  /** String prefix used in the `[[toString]]` method. */
  def stringPrefix: String

}
