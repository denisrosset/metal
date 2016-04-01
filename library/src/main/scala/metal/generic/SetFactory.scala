package metal
package generic

trait SetFactory {

  type Extra[_]

  type S[K] <: generic.Set[K]

  def empty[K:MetalTag:Extra]: S[K]

  def apply[K:MetalTag:Extra](items: K*): S[K]

  def fromArray[K:MetalTag:Extra](array: Array[K]): S[K]

  def fromIterable[K:MetalTag:Extra](items: Iterable[K]): S[K]

}
