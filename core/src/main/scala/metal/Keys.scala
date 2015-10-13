package metal

import scala.{specialized => sp}

trait Keys[K] extends Pointable {

  /** Returns the key pointed by `ptr`. */
  def ptrKey(ptr: VPtr[Tag]): K

  def ptrKeyP(ptr: VPtr[Tag])(implicit p: Primitive[K]): Long

}

trait AddKeys[K] extends Keys[K] {

  /** Adds the given key to the collection. If the key is already present, returns the pointer
    * associated with the existing key.
    * If the key is new, any eventual associated values are undefined (they should immediately
    * be updated).
    */
  def ptrAddKey(key: K): VPtr[Tag]

  def ptrAddKeyP(key: Long)(implicit p: Primitive[K]): VPtr[Tag]

}
