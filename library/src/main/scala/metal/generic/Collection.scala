package metal
package generic

abstract class Collection extends Enumerable { lhs =>

  /** Generic type of this collection (e.g. Map, Map2, Set). */
  type Generic >: lhs.type <: Collection

  /** Mutable variant of this collection. */
  type Mutable <: mutable.Collection

  /** Immutable variant of this collection. */
  type Immutable <: immutable.Collection

  type Scala <: scala.collection.immutable.Iterable[_]

  /** Creates a mutable copy of this collection. */
  def mutableCopy: Mutable

  /** Returns this collection if already immutable, or an immutable copy. */
  def toImmutable: Immutable

  /** Returns a Scala collection that wraps this collection, creating an immutable
    * copy if this collection is mutable.
    */
  def toScala: Scala

  /** String prefix used in the `[[toString]]` method. */
  def stringPrefix: String

}
