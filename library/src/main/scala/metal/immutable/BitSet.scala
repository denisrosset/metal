package metal
package immutable

trait BitSet[K] extends generic.BitSet[K] with immutable.SortedSet[K]

final class BitSetImpl(val words: Array[Long], val nWords: Int) extends generic.BitSetImpl with immutable.BitSet[Int]
