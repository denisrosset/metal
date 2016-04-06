package metal
package immutable

final class BitSet(val words: Array[Long]) extends generic.BitSet with immutable.SortedSet[Int] {

  def nWords = words.length

  def toScala = scala.collection.immutable.BitSet.fromBitMaskNoCopy(words)

}

object BitSet extends SetBuilder[Int, immutable.BitSet] {

  type MSK = mutable.BitSet

  def mutableBuilder = mutable.BitSet

  override def fromIterable(items: Iterable[Int]): immutable.BitSet = items match {
    case bs1: scala.collection.immutable.BitSet.BitSet1 => new immutable.BitSet(Array(bs1.elems))
    case bsn: scala.collection.immutable.BitSet.BitSetN => new immutable.BitSet(bsn.elems)
    case bs: scala.collection.BitSet => new immutable.BitSet(bs.toBitMask)
    case _ => super.fromIterable(items)
  }

}
