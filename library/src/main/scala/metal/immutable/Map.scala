package metal
package immutable

trait Map[K, V] extends generic.Map[K, V] with immutable.Collection {

  type Immutable >: this.type <: immutable.Map[K, V]

}
