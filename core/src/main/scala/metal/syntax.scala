package metal

import spire.util.Opt

abstract class LowerPrioritySyntax {

  implicit def loopOps[E](lhs: Enumerable with Elements[E]): LoopOpsE[E] = new LoopOpsE[E](lhs)

}

object syntax extends LowerPrioritySyntax {

  implicit def searchableOps[K](lhs: Searchable[K]): SearchableOps[K] = new SearchableOps(lhs)

  implicit def removableSearchableOps[K](lhs: Searchable[K] with Removable): RemovableSearchableOps[K, lhs.type] = new RemovableSearchableOps[K, lhs.type](lhs)

  implicit def addOps[K](lhs: AddKeys[K] with Elements[K]): AddOps[K, lhs.type] = new AddOps[K, lhs.type](lhs)

  implicit def searchableAddOps[K](lhs: AddKeys[K] with Searchable[K] with Elements[K]): SearchableAddOps[K] = new SearchableAddOps(lhs)

  implicit def updateOps[K, V](lhs: AddKeys[K] with Updatable[V]): UpdateOps[K, V] = new UpdateOps[K, V](lhs)

  implicit def searchableValuesOps[K, V](lhs: Searchable[K] with Values[V]): SearchableValuesOps[K, V] = new SearchableValuesOps[K, V](lhs)

  implicit def loopOpsK[K](lhs: Enumerable with Keys[K] with Elements[K]): LoopOpsK[K] = new LoopOpsK[K](lhs)

  implicit def loopOpsKV[K, V](lhs: Enumerable with Keys[K] with Values[V]): LoopOpsKV[K, V] = new LoopOpsKV[K, V](lhs)

  implicit def loopOpsKV1V2[K, V1, V2](lhs: Enumerable with Keys[K] with Values1[V1] with Values2[V2]): LoopOpsKV1V2[K, V1, V2] = new LoopOpsKV1V2[K, V1, V2](lhs)

  implicit def update2Ops[K, V1, V2](lhs: AddKeys[K] with Updatable1[V1] with Updatable2[V2]): Update2Ops[K, V1, V2] = new Update2Ops[K, V1, V2](lhs)

  implicit def searchableValues1Ops[K, V1](lhs: Searchable[K] with Values1[V1]): SearchableValues1Ops[K, V1] = new SearchableValues1Ops[K, V1](lhs)

  implicit def searchableValues2Ops[K, V2](lhs: Searchable[K] with Values2[V2]): SearchableValues2Ops[K, V2] = new SearchableValues2Ops[K, V2](lhs)

}
