package metal

trait Values2[V2] extends Pointable {

  /** Returns the value of the object pointed by the pointer. */
  def ptrValue2[@specialized W2](ptr: VPtr[Tag]): W2

}

trait Updatable2[V2] extends Values2[V2] {

  def ptrUpdate2[@specialized W2](ptr: VPtr[Tag], v: W2): Unit

}
