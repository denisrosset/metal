package metal.immutable

import scala.reflect.ClassTag
import metal.Methods

trait SetFactory extends metal.SetFactory {

  type S[K] <: metal.immutable.Set[K]
  type M[K] <: metal.mutable.Set[K] { type Immutable <: S[K] }

  def mutableFactory: metal.mutable.SetFactory { type S[K] = M[K]; type Extra[K] = SetFactory.this.Extra[K] }

  def empty[A:Methods:Extra]: S[A] = mutableFactory.empty[A].result()

  def apply[A:Methods:Extra](items: A*): S[A] = mutableFactory.apply(items: _*).result()

  def fromArray[A:Methods:Extra](array: Array[A]): S[A] = mutableFactory.fromArray(array).result()

}
