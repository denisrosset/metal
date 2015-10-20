package metal

import scala.annotation.{switch, tailrec}
import scala.reflect.ClassTag

import spire.algebra.Order
import spire.syntax.cfor._
import spire.util.Opt

trait FHashMap[K, V] extends FMap[K, V] {

  final type IType = IHashMap[K, V]
  final type MType = MHashMap[K, V]

  def len: Int
  def used: Int
  def mask: Int
  def limit: Int

  def mutableCopy(): MHashMap[K, V]

}

trait IHashMap[K, V] extends FHashMap[K, V] with IMap[K, V] {

}

trait MHashMap[K, V] extends FHashMap[K, V] with MMap[K, V] {

  def keys: Array[K]
  def buckets: Array[Byte]
  def values: Array[V]

  def result(): IHashMap[K, V]

}

object HashMap extends MapFactory[Any, Dummy, Any] {

  import impl.HashMapImpl

  def empty[K, V](implicit K: Methods[K], d: Dummy[K], e: KLBEv[K], V: Methods[V]): MHashMap[K, V] = ofSize(0)(K, d, e, V)

  /** Creates a HashMap that can hold n unique keys without resizing itself.
    *
    * Note that the internal representation will allocate more space
    * than requested to satisfy the requirements of internal
    * alignment. Map uses arrays whose lengths are powers of two, and
    * needs at least 33% of the map free to enable good hashing
    * performance.
    * 
    * Example: HashMap.ofSize[Int, String](100).
    */

  def ofSize[K:Methods:Dummy:KLBEv, V:Methods](n: Int): MHashMap[K, V] = ofAllocatedSize(n / 2 * 3)

  /** Allocates an empty HashMap, with underlying storage of size n.
    * 
    * This method is useful if you know exactly how big you want the
    * underlying array to be. In most cases ofSize() is probably what
    * you want instead.
    */
  private[metal] def ofAllocatedSize[K, V](n: Int)(implicit K: Methods[K], V: Methods[V]) = {
    import K.{classTag => ctK}
    import V.{classTag => ctV}
    val sz = Util.nextPowerOfTwo(n) match {
      case n if n < 0 => sys.error(s"Bad allocated size $n for collection")
      case 0 => 8
      case n => n
    }
    new HashMapImpl[K, V](
      keys = new Array[K](sz),
      values = new Array[V](sz),
      buckets = new Array[Byte](sz),
      len = 0,
      used = 0,
      mask = sz - 1,
      limit = (sz * 0.65).toInt)
  }

}
