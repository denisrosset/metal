package metal

import scala.annotation.{switch, tailrec}
import scala.{specialized => sp}
import scala.reflect.ClassTag

import spire.algebra.Order
import spire.syntax.cfor._
import spire.util.Opt

trait FHashMap2[K, V1, V2] extends FMap2[K, V1, V2] {

  type IType = IHashMap2[K, V1, V2]
  type MType = MHashMap2[K, V1, V2]

  def mutableCopy(): MHashMap2[K, V1, V2]

}

trait IHashMap2[K, V1, V2] extends FHashMap2[K, V1, V2] with IMap2[K, V1, V2]

trait MHashMap2[K, V1, V2] extends FHashMap2[K, V1, V2] with MMap2[K, V1, V2] {

  def len: Int
  def used: Int
  def mask: Int
  def limit: Int
  def keys: Array[K]
  def buckets: Array[Byte]
  def values1: Array[V1]
  def values2: Array[V2]

  def result(): IHashMap2[K, V1, V2]

}

object MHashMap2 extends MMap2Factory[Any, Dummy, Any, Any, MHashMap2] {

  import impl.HashMap2Impl

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

  def ofSize[K:Methods:Dummy:KLBEv, V1:Methods, V2:Methods](n: Int): MHashMap2[K, V1, V2] = HashMap2Impl.ofAllocatedSize(n / 2 * 3)

}
