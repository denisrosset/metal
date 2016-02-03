package metal

trait Removable extends Nextable {

  /** Removes the pointed element. */
  def ptrRemove(ptr: VPtr[Removable.this.type]): Unit

  /** Removes the pointed element, and returns the pointer
    * to the next element, or null if at the end.
    */
  def ptrRemoveAndAdvance(ptr: VPtr[Removable.this.type]): Ptr[Removable.this.type]

  def clear(): Unit

}
