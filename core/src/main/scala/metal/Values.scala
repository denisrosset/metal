package metal

trait Values[V] extends Pointable {

  type Cap <: Values[V]

  /** Returns the value of the object pointed by the pointer. */
  def ptrValue[@specialized W](ptr: MyVPtr): W

}
