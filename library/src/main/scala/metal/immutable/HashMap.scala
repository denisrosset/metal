package metal
package immutable

import scala.collection.AbstractIterator
import scala.reflect.ClassTag

import metal.syntax._

final class WrappedHashMap[K, V](val w: metal.immutable.HashMap[K, V])
    extends scala.collection.immutable.Map[K, V]
    with scala.collection.MapLike[K, V, WrappedHashMap[K, V]]
    with WrappedMap[K, V] {

  import w.{ctK, ctV}

  override def empty: WrappedHashMap[K, V] = new WrappedHashMap[K, V](metal.immutable.HashMap.empty[K, V])

  def +[B1 >: V](kv: (K, B1)): scala.collection.immutable.Map[K, B1] = {
    val b = scala.collection.immutable.HashMap.newBuilder[K, B1]
    w.foreach { (k, v) => b += ((k, v)) }
    b += kv
    b.result()
  }

  def -(key: K): WrappedHashMap[K, V] =
    if (!w.contains(key)) this else {
      val b = w.mutableCopy
      b -= key
      new WrappedHashMap[K, V](b.result())
    }

  def get(key: K): Option[V] = w.ptrFind(key) match {
    case IsVPtr(vp) => Some(w.ptrValue(vp))
    case _ => None
  }

  def iterator: Iterator[(K, V)] = new AbstractIterator[(K, V)] {
    private[this] var ptr: Ptr[w.type] = w.ptr
    def hasNext = ptr.nonNull
    def next(): (K, V) = ptr match {
      case IsVPtr(vp) =>
        ptr = w.ptrNext(vp)
        (w.ptrKey(vp), w.ptrValue(vp))
      case _ => Iterator.empty.next
    }
  }

}

final class HashMap[K, V](
  private[metal] val keys: Array[K],
  private[metal] val buckets: Array[Byte],
  private[metal] val values : Array[V],
  val size: Int,
  val used: Int,
  val mask: Int,
  val limit: Int
)(implicit
  val ctK: ClassTag[K],
  val K: MetalTag[K],
  val ctV: ClassTag[V],
  val V: MetalTag[V]
) extends generic.HashMap[K, V] with immutable.Map[K, V] {

  def toScala = new WrappedHashMap[K, V](this)

}

object HashMap extends immutable.MapFactory {

  type KExtra[K] = metal.util.Dummy[K]
  type VExtra[V] = metal.util.Dummy[V]

  type M[K, V] = metal.immutable.HashMap[K, V]
  type MM[K, V] = metal.mutable.HashMap[K, V]

  def mutableFactory = metal.mutable.HashMap

}
