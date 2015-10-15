package metal

trait Keys[K] extends Pointable {

  /** Returns the key pointed by `ptr`. */
  def ptrKey[@specialized L](ptr: VPtr[Tag]): L

}

trait AddKeys[K] extends Keys[K] {

  /** Adds the given key to the collection. If the key is already present, returns the pointer
    * associated with the existing key.
    * If the key is new, any eventual associated values are undefined (they should immediately
    * be updated).
    */
  def ptrAddKey[@specialized L](key: L): VPtr[Tag]

}
