package metal

trait Countable extends Nextable {

  /** Returns a pointer to the start of this collection instance. */
  def ptrStart: Ptr[Tag]

  /** Returns true if the collection is empty, false otherwise. */
  def isEmpty: Boolean

  /** Returns true if the collection is non-empty, false otherwise. */
  def nonEmpty: Boolean

  /** Return the size of this collection as an Int.
    * 
    * Since most collections use arrays, their size is limited to what a 32-bit
    * signed integer can represent.
    */
  def size: Int

}
