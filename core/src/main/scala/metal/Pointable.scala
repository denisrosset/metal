package metal

/** Trait for containers that can have pointers. */
trait Pointable {

  /** Tag of this instance, used to create path dependent types that
    * attach a pointable container to its pointers.
    */
  trait Tag

  /** Capabilities of this container. */
  type Cap <: Pointable

  /** Own pointer type. */
  type MyPtr = Ptr[Tag, Cap]
  /** Own valid pointer type. */
  type MyVPtr = VPtr[Tag, Cap]

}
