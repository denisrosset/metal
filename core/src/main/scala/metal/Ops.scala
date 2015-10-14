package metal

import spire.util.Opt

// missing: foreach, forall, exists
// missing: variants of fold
// missing: 
trait Operations2 {

  implicit def searchableOps[K](lhs: Searchable[K]): SearchableOps[K] = new SearchableOps(lhs)

  implicit def removableSearchableOps[K](lhs: Searchable[K] with Removable[K]): RemovableSearchableOps[K] = new RemovableSearchableOps(lhs)

  implicit def addOps[K](lhs: AddKeys[K] with NoValues): AddOps[K, lhs.type] = new AddOps[K, lhs.type](lhs)

  implicit def searchableAddOps[K](lhs: AddKeys[K] with Searchable[K] with NoValues): SearchableAddOps[K] = new SearchableAddOps(lhs)

  implicit def updateOps[K, V](lhs: AddKeys[K] with Updatable[V]): UpdateOps[K, V] = new UpdateOps[K, V](lhs)

  implicit def searchableValuesOps[K, V](lhs: Searchable[K] with Values[V]): SearchableValuesOps[K, V] = new SearchableValuesOps[K, V](lhs)

}

trait Operations1 extends Operations2 {


}

trait Operations extends Operations1 {

  implicit def searchableOpsP[K](lhs: Searchable[K])(implicit ev: Primitive[K]) = new SearchableOpsP[K](lhs)(ev)

  implicit def removableSearchableOpsP[K](lhs: Searchable[K] with Removable[K])(implicit ev: Primitive[K]): RemovableSearchableOpsP[K] = new RemovableSearchableOpsP(lhs)

  implicit def addOpsP[K](lhs: AddKeys[K] with NoValues)(implicit ev: Primitive[K]): AddOpsP[K, lhs.type] = new AddOpsP[K, lhs.type](lhs)

  implicit def searchableAddOpsP[K](lhs: AddKeys[K] with Searchable[K] with NoValues)(implicit ev: Primitive[K]): SearchableAddOps[K] = new SearchableAddOps(lhs)

  implicit def updateOpsP[K, V](lhs: AddKeys[K] with Updatable[V])(implicit evK: Primitive[K], evV: Primitive[V]): UpdateOpsP[K, V] = new UpdateOpsP[K, V](lhs)

  implicit def searchableValuesOpsP[K, V](lhs: Searchable[K] with Values[V])(implicit evK: Primitive[K], evV: Primitive[V]): SearchableValuesOpsP[K, V] = new SearchableValuesOpsP[K, V](lhs)

}
