package metal
package immutable

import scala.reflect.{classTag, ClassTag}

final class WrappedBuffer[V](val w: metal.immutable.Buffer[V])
    extends scala.collection.immutable.IndexedSeq[V]
    with scala.collection.IndexedSeqOptimized[V, WrappedBuffer[V]] {

  import w.ctV

  def apply(idx: Int): V = w(idx)

  def length: Int = w.length

  override def newBuilder: scala.collection.mutable.Builder[V, WrappedBuffer[V]] = new scala.collection.mutable.Builder[V, WrappedBuffer[V]] {
    private[this] var current: metal.mutable.Buffer[V] = metal.mutable.Buffer.empty[V]
    def clear() = { current = metal.mutable.Buffer.empty[V] }
    def +=(elem: V) = { current += elem; this }
    def result() = new WrappedBuffer[V](current.result())
  }
  
}

final class Buffer[@specialized V](private[metal] val array: Array[V], val length: Int)(implicit val V: MetalTag[V], val ctV: ClassTag[V]) extends generic.Buffer[V] with immutable.Collection {

  @inline final def apply(idx: Int): V = array(idx)

  def toScala: WrappedBuffer[V] = new WrappedBuffer[V](this)

}

object Buffer extends generic.BufferFactory {

  val startSize = 8

  def empty[@specialized V:ClassTag]: immutable.Buffer[V] = new immutable.Buffer[V](classTag[V].newArray(0), 0)

  def apply[@specialized V:ClassTag](items: V*): immutable.Buffer[V] = {
    val array = classTag[V].newArray(items.size)
    val it = items.iterator
    var i = 0
    while (it.hasNext) {
      array(i) = it.next
      i += 1
    }
    new immutable.Buffer[V](array, array.length)
  }

  def fromArray[@specialized V:ClassTag](array: Array[V]): immutable.Buffer[V] =
    new immutable.Buffer[V](array.clone, array.length)

  def fromIterable[@specialized V:ClassTag](iterable: Iterable[V]): immutable.Buffer[V] = {
    val array = classTag[V].newArray(iterable.size)
    val it = iterable.iterator
    var i = 0
    while (it.hasNext) {
      array(i) = it.next
      i += 1
    }
    new immutable.Buffer[V](array, array.length)
  }

}
