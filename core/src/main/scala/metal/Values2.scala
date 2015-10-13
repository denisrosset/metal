package metal

trait Values2[V2] extends Pointable {

  /** Returns the value of the object pointed by the pointer. */
  def ptrValue2(ptr: VPtr[Tag]): V2

  def ptrValue2P(ptr: VPtr[Tag])(implicit p: Primitive[V2]): Long

}

trait Updatable2[V2] extends Values2[V2] {

  def ptrUpdate2(ptr: VPtr[Tag], v: V2): Unit

  def ptrUpdate2P(ptr: VPtr[Tag], v: Long)(implicit p: Primitive[V2]): Unit

}
