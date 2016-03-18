package metal

import spire.util.Opt

object syntax {

  implicit def searchableOps[K](lhs: Searchable[K]): SearchableOps[K] = new SearchableOps(lhs)

  implicit def removableSearchableOps[K](lhs: Searchable[K] with Removable): RemovableSearchableOps[K, lhs.type] = new RemovableSearchableOps[K, lhs.type](lhs)

  implicit def addOps[K](lhs: AddKeys[K] with Elements1[K]): AddOps[K, lhs.type] = new AddOps[K, lhs.type](lhs)

  implicit def searchableAddOps[K](lhs: AddKeys[K] with Searchable[K] with Elements1[K]): SearchableAddOps[K] = new SearchableAddOps(lhs)

  implicit def updateOps[K, V](lhs: AddKeys[K] with Updatable[V]): UpdateOps[K, V] = new UpdateOps[K, V](lhs)

  implicit def searchableValuesOps[K, V](lhs: Searchable[K] with Values[V]): SearchableValuesOps[K, V] = new SearchableValuesOps[K, V](lhs)

  implicit def loopOps1[E1](lhs: Enumerable with NElements1[E1]): LoopOps1[E1] = new LoopOps1[E1](lhs)

  implicit def loopOps2[E1, E2](lhs: Enumerable with NElements2[E1, E2]): LoopOps2[E1, E2] = new LoopOps2[E1, E2](lhs)

  implicit def loopOps3[E1, E2, E3](lhs: Enumerable with NElements3[E1, E2, E3]): LoopOps3[E1, E2, E3] = new LoopOps3[E1, E2, E3](lhs)

  implicit def update2Ops[K, V1, V2](lhs: AddKeys[K] with Updatable1[V1] with Updatable2[V2]): Update2Ops[K, V1, V2] = new Update2Ops[K, V1, V2](lhs)

  implicit def searchableValues12Ops[K, V1, V2](lhs: Searchable[K] with Values1[V1] with Values2[V2]): SearchableValues12Ops[K, V1, V2] = new SearchableValues12Ops[K, V1, V2](lhs)

  implicit def searchableValues1Ops[K, V1](lhs: Searchable[K] with Values1[V1]): SearchableValues1Ops[K, V1] = new SearchableValues1Ops[K, V1](lhs)

  implicit def searchableValues2Ops[K, V2](lhs: Searchable[K] with Values2[V2]): SearchableValues2Ops[K, V2] = new SearchableValues2Ops[K, V2](lhs)

}
