package metal

import scala.reflect.ClassTag

trait SetFactory {

  type Extra[_]

  type S[K] <: metal.Set[K]

  def empty[K:Methods:Extra]: S[K]

  def apply[K:Methods:Extra](items: K*): S[K]

  def fromArray[K:Methods:Extra](array: Array[K]): S[K]

  def fromIterable[K:Methods:Extra](items: Iterable[K]): S[K]

}
