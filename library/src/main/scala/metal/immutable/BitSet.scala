package metal
package immutable

import scala.collection.immutable.{BitSet => ScalaBitSet}
final class BitSet(val words: Array[Long]) extends generic.BitSet with immutable.SortedSet[Int] {

  def nWords = words.length

}

object BitSet extends SetBuilder[Int, immutable.BitSet] {

  type MK = mutable.BitSet

  def mutableBuilder = mutable.BitSet

  override def fromIterable(items: Iterable[Int]): immutable.BitSet = items match {
    case bs1: ScalaBitSet.BitSet1 => new immutable.BitSet(Array(bs1.elems))
    case bsn: ScalaBitSet.BitSetN => new immutable.BitSet(bsn.elems)
    case bs: scala.collection.BitSet => new immutable.BitSet(bs.toBitMask)
    case _ => super.fromIterable(items)
  }

}
