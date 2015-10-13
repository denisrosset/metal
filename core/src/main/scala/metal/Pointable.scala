package metal

trait Pointable {

  /** Tag of this instance, used to create path dependent types that
    * attach a pointable collection to its pointers.
    */
  trait Tag

}
