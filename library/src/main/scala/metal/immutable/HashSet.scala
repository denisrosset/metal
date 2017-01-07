package metal
package immutable

import scala.reflect.ClassTag

import metal.syntax._

final class WrappedHashSet[K](val w: metal.immutable.HashSet[K])
    extends scala.collection.immutable.Set[K]
    with scala.collection.SetLike[K, WrappedHashSet[K]]
    with WrappedSet[K] {

  import w.ctK

  override def empty: WrappedHashSet[K] = new WrappedHashSet[K](metal.immutable.HashSet.empty[K])

  def -(elem: K): WrappedHashSet[K] =
    if (!w.contains(elem)) this else {
      val b = w.mutableCopy
      b -= elem
      new WrappedHashSet(b.result())
    }

  def +(elem: K): WrappedHashSet[K] =
    if (w.contains(elem)) this else {
      val b = w.mutableCopy
      b += elem
      new WrappedHashSet(b.result())
    }

  override def newBuilder: scala.collection.mutable.Builder[K, WrappedHashSet[K]] = new scala.collection.mutable.Builder[K, WrappedHashSet[K]] {
    private[this] var current: metal.mutable.HashSet[K] = metal.mutable.HashSet.empty[K]
    def clear() = { current = metal.mutable.HashSet.empty[K] }
    def +=(elem: K) = { val c = current; c += elem; this }
    def result() = new WrappedHashSet[K](current.result())
  }

  def iterator: Iterator[K] = new Iterator[K] {
    private[this] var ptr: Ptr[w.type] = w.ptr
    def hasNext = ptr.nonNull
    def next(): K = ptr match {
      case IsVPtr(vp) =>
        ptr = w.ptrNext(vp)
        w.ptrKey(vp)
      case _ => Iterator.empty.next
    }
  }

  def contains(elem: K) = w.contains(elem)

}

final class HashSet[K](
  private[metal] val keys: Array[K],
  private[metal] val buckets: Array[Byte],
  val size: Int,
  val used: Int,
  val mask: Int,
  val limit: Int
)(implicit
  val ctK: ClassTag[K],
  val K: MetalTag[K]
) extends generic.HashSet[K] with immutable.Set[K] {

  def toScala = new WrappedHashSet[K](this)

}

object HashSet extends immutable.SetFactory {

  type Extra[K] = metal.util.Dummy[K]
  type S[K] = metal.immutable.HashSet[K]
  type MS[K] = metal.mutable.HashSet[K]

  def mutableFactory = metal.mutable.HashSet

}
