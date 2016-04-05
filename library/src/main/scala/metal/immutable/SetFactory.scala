package metal
package immutable

import scala.reflect.ClassTag

trait SetFactory extends generic.SetFactory {

  type S[K] <: immutable.Set[K]
  type M[K] <: mutable.Set[K] { type Immutable <: S[K] }

  def mutableFactory: mutable.SetFactory { type S[K] = M[K]; type Extra[K] = SetFactory.this.Extra[K] }

  def empty[A:ClassTag:Extra]: S[A] = mutableFactory.empty[A].result()

  def apply[A:ClassTag:Extra](items: A*): S[A] = mutableFactory.apply(items: _*).result()

  def fromArray[A:ClassTag:Extra](array: Array[A]): S[A] = mutableFactory.fromArray(array).result()

  def fromIterable[A:ClassTag:Extra](items: Iterable[A]): S[A] = mutableFactory.fromIterable(items).result()

}
