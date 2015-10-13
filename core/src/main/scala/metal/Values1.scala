package metal

trait Values1[V1] extends Pointable {

  /** Returns the value of the object pointed by the pointer. */
  def ptrValue1(ptr: VPtr[Tag]): V1

  def ptrValue1P(ptr: VPtr[Tag])(implicit p: Primitive[V1]): Long

}

trait Updatable1[V1] extends Values1[V1] {

  def ptrUpdate1(ptr: VPtr[Tag], v: V1): Unit

  def ptrUpdate1P(ptr: VPtr[Tag], v: Long)(implicit p: Primitive[V1]): Unit

}
