package metal

trait Values2[V2] extends Pointable {

  type Cap <: Values2[V2]

  /** Returns the value of the object pointed by the pointer. */
  def ptrValue2[@specialized W2](ptr: MyVPtr): W2

}
