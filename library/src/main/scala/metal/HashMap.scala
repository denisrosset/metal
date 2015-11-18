package metal

import scala.annotation.{switch, tailrec}
import scala.reflect.ClassTag

import spire.algebra.Order
import spire.syntax.cfor._
import spire.util.Opt

trait FHashMap[K, V] extends FMap[K, V] {

  type IType = IHashMap[K, V]
  type MType = MHashMap[K, V]

  def mutableCopy(): MHashMap[K, V]

}

trait IHashMap[K, V] extends FHashMap[K, V] with IMap[K, V]

trait MHashMap[K, V] extends FHashMap[K, V] with MMap[K, V] {

  def len: Int
  def used: Int
  def mask: Int
  def limit: Int
  def keys: Array[K]
  def buckets: Array[Byte]
  def values: Array[V]

  def result(): IHashMap[K, V]

}

object MHashMap extends MMapFactory[Any, Dummy, Any, MHashMap] {

  import impl.HashMapImpl

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

  def ofSize[K:Methods:Dummy:KLBEv, V:Methods](n: Int): MHashMap[K, V] = HashMapImpl.ofAllocatedSize(n / 2 * 3)

}
