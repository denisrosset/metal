package metal

import spire.util.Opt

object syntax {

  implicit def searchableOps[K](lhs: Searchable[K]): SearchableOps[K] = new SearchableOps(lhs)

  implicit def removableSearchableOps[K](lhs: Searchable[K] with Removable[K]): RemovableSearchableOps[K, lhs.type] = new RemovableSearchableOps[K, lhs.type](lhs)

  implicit def addOps[K](lhs: AddKeys[K] with ShapeK): AddOps[K, lhs.type] = new AddOps[K, lhs.type](lhs)

  implicit def searchableAddOps[K](lhs: AddKeys[K] with Searchable[K] with ShapeK): SearchableAddOps[K] = new SearchableAddOps(lhs)

  implicit def updateOps[K, V](lhs: AddKeys[K] with Updatable[V]): UpdateOps[K, V] = new UpdateOps[K, V](lhs)

  implicit def update2Ops[K, V1, V2](lhs: AddKeys[K] with Updatable1[V1] with Updatable2[V2]): Update2Ops[K, V1, V2] = new Update2Ops[K, V1, V2](lhs)

  implicit def searchableValuesOps[K, V](lhs: Searchable[K] with Values[V]): SearchableValuesOps[K, V] = new SearchableValuesOps[K, V](lhs)

  implicit def searchableValues1Ops[K, V1](lhs: Searchable[K] with Values1[V1]): SearchableValues1Ops[K, V1] = new SearchableValues1Ops[K, V1](lhs)

  implicit def searchableValues2Ops[K, V2](lhs: Searchable[K] with Values2[V2]): SearchableValues2Ops[K, V2] = new SearchableValues2Ops[K, V2](lhs)

  // loops

  implicit def opsK[K](lhs: ShapeK with Keys[K] with Nextable with Countable): OpsK[K] = new OpsK[K](lhs)

  implicit def opsV[V](lhs: ShapeV with Values[V] with Nextable with Countable): OpsV[V] = new OpsV[V](lhs)

  implicit def opsKV[K, V](lhs: ShapeKV with Keys[K] with Values[V] with Nextable with Countable): OpsKV[K, V] = new OpsKV[K, V](lhs)

  implicit def opsKV1V2[K, V1, V2](lhs: ShapeKV1V2 with Keys[K] with Values1[V1] with Values2[V2] with Nextable with Countable): OpsKV1V2[K, V1, V2] = new OpsKV1V2[K, V1, V2](lhs)

}
