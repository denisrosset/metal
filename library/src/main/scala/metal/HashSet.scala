package metal

abstract class HashSet[K] extends metal.Set[K] {

  type Immutable = metal.immutable.HashSet[K]
  type Mutable = metal.mutable.HashSet[K]

  def len: Int
  def used: Int
  def mask: Int
  def limit: Int
  def key(i: Int): K
  def bucket(i: Int): Byte

}

trait HashSetFactory extends SetFactory {

  type Extra[K] = Dummy[K]

  type S[K] <: metal.HashSet[K]

}
