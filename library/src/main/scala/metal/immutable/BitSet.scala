package metal.immutable

trait BitSet[K] extends metal.BitSet[K] with metal.immutable.SortedSet[K]

final class BitSetImpl(val words: Array[Long], val nWords: Int) extends metal.BitSetImpl with metal.immutable.BitSet[Int]
