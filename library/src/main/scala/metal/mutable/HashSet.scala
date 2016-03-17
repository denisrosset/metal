package metal.mutable

object HashSetFactory extends metal.HashSetFactory with metal.mutable.SetFactory {

  type S[K] = metal.mutable.HashSet[K]

  def ofAllocatedSize[K:Methods:Extra](n: Int): S[K]

  def ofSize[K:Methods:Dummy:LBEv](n: Int): MHashSet[K] = ofAllocatedSize(n / 2 * 3)

}
