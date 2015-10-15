package metal

trait Values[V] extends Pointable {

  /** Returns the value of the object pointed by the pointer. */
  def ptrValue[@specialized W](ptr: VPtr[Tag]): W

}
