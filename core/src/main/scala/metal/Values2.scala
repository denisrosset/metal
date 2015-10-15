package metal

trait Values2[V2] extends Pointable {

  /** Returns the value of the object pointed by the pointer. */
  def ptrValue2[@specialized W2](ptr: VPtr[Tag]): W2

}
