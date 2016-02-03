package metal

/* Trait for containers whose elements can be enumerated. 
 * Equivalent to scala.container.Iterable, but for pointers.
 */
trait Enumerable extends Nextable {

  /** Returns a pointer to the start of this container instance. */
  def ptr: Ptr[Enumerable.this.type]

  /** Returns true if the container is empty, false otherwise. */
  def isEmpty: Boolean

  /** Returns true if the container is non-empty, false otherwise. */
  def nonEmpty: Boolean

  /** Return the size of this container; we use `Long` so that big containers
    * can be manipulated. */
  def longSize: Long

}
