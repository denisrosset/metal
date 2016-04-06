package metal
package immutable

import scala.collection.AbstractIterator
import scala.reflect.ClassTag

import metal.syntax._

final class WrappedHashMap2[K, V1, V2](val w: metal.immutable.HashMap2[K, V1, V2])
    extends scala.collection.immutable.Map[K, (V1, V2)]
    with scala.collection.MapLike[K, (V1, V2), WrappedHashMap2[K, V1, V2]]
    with WrappedMap2[K, V1, V2] {

  import w.{ctK, ctV1, ctV2}

  override def empty: WrappedHashMap2[K, V1, V2] =
    new WrappedHashMap2[K, V1, V2](metal.immutable.HashMap2.empty[K, V1, V2])

  def +[B1 >: (V1, V2)](kv1v2: (K, B1)): scala.collection.immutable.Map[K, B1] = {
    val b = scala.collection.immutable.HashMap.newBuilder[K, B1]
    w.foreach { (k, v1, v2) => b += ((k, ((v1, v2)))) }
    b += kv1v2
    b.result()
  }

  def -(key: K): WrappedHashMap2[K, V1, V2] =
    if (!w.contains(key)) this else {
      val b = w.mutableCopy
      b -= key
      new WrappedHashMap2[K, V1, V2](b.result())
    }

  def get(key: K): Option[(V1, V2)] = w.ptrFind(key) match {
    case IsVPtr(vp) => Some((w.ptrValue1(vp), w.ptrValue2(vp)))
    case _ => None
  }

  def iterator: Iterator[(K, (V1, V2))] = new AbstractIterator[(K, (V1, V2))] {
    private[this] var ptr: Ptr[w.type] = w.ptr
    def hasNext = ptr.nonNull
    def next(): (K, (V1, V2)) = ptr match {
      case IsVPtr(vp) =>
        ptr = w.ptrNext(vp)
        (w.ptrKey(vp), (w.ptrValue1(vp), w.ptrValue2(vp)))
      case _ => Iterator.empty.next
    }
  }

}

final class HashMap2[K, V1, V2](
  private[metal] val keys: Array[K],
  private[metal] val buckets: Array[Byte],
  private[metal] val values1: Array[V1],
  private[metal] val values2: Array[V2],
  val size: Int,
  val used: Int,
  val mask: Int,
  val limit: Int
)(implicit
  val ctK: ClassTag[K],
  val K: MetalTag[K],
  val ctV1: ClassTag[V1],
  val V1: MetalTag[V1],
  val ctV2: ClassTag[V2],
  val V2: MetalTag[V2]
) extends generic.HashMap2[K, V1, V2]
    with immutable.Map2[K, V1, V2] {

  def toScala = new WrappedHashMap2[K, V1, V2](this)

}

object HashMap2 extends immutable.Map2Factory {

  type KExtra[K] = metal.util.Dummy[K]
  type V1Extra[V1] = metal.util.Dummy[V1]
  type V2Extra[V2] = metal.util.Dummy[V2]

  type M[K, V1, V2] = metal.immutable.HashMap2[K, V1, V2]
  type MM[K, V1, V2] = metal.mutable.HashMap2[K, V1, V2]

  def mutableFactory = metal.mutable.HashMap2

}
