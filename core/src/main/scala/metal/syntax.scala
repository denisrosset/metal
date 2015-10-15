package metal

import spire.util.Opt

// missing: foreach, forall, exists
// missing: variants of fold
// missing: 
object syntax {

  @inline final implicit def searchableOps[K](lhs: Searchable[K]): SearchableOps[K] = new SearchableOps(lhs)

  @inline final implicit def removableSearchableOps[K](lhs: Searchable[K] with Removable[K]): RemovableSearchableOps[K, lhs.type] = new RemovableSearchableOps[K, lhs.type](lhs)

  @inline final implicit def addOps[K](lhs: AddKeys[K] with NoValues): AddOps[K, lhs.type] = new AddOps[K, lhs.type](lhs)

  @inline final implicit def searchableAddOps[K](lhs: AddKeys[K] with Searchable[K] with NoValues): SearchableAddOps[K] = new SearchableAddOps(lhs)

  @inline final implicit def updateOps[K, V](lhs: AddKeys[K] with Updatable[V]): UpdateOps[K, V] = new UpdateOps[K, V](lhs)

  @inline final implicit def searchableValuesOps[K, V](lhs: Searchable[K] with Values[V]): SearchableValuesOps[K, V] = new SearchableValuesOps[K, V](lhs)

}
