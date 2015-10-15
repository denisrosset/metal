package metal

trait Keys[K] extends Pointable {

  /** Returns the key pointed by `ptr`. */
  def ptrKey[@specialized L](ptr: VPtr[Tag]): L

}
