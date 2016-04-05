package metal
package generic

import scala.reflect.ClassTag

trait SetFactory {

  type Extra[_]

  type S[K] <: generic.Set[K]

  def empty[K:ClassTag:Extra]: S[K]

  def apply[K:ClassTag:Extra](items: K*): S[K]

  def fromArray[K:ClassTag:Extra](array: Array[K]): S[K]

  def fromIterable[K:ClassTag:Extra](items: Iterable[K]): S[K]

}
