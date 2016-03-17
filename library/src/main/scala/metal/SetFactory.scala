package metal

import scala.reflect.ClassTag
import spire.syntax.cfor._

trait SetFactory {

  type Extra[_]

  type S[K] <: metal.Set[K]

  def empty[K:Methods:Extra]: S[K]

  def apply[K:Methods:Extra](items: K*): S[K]

  def fromArray[K:Methods:Extra](array: Array[K]): S[K]

}
