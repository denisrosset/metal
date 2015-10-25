package metal

/** Trait for containers whose pointers can be iterated over. Note that these can
  * contain multiple parts that can be iterated independently; if there is only
  * one part, the trait to extend is `Enumerable`.
  */
trait Nextable extends Pointable {

  /** Returns a pointer to the next element if available,
    * or returns a null pointer.
    */
  def ptrNext(ptr: VPtr[Tag]): Ptr[Tag]

}
