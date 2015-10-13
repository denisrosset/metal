package metal

trait Values[V] extends Pointable {

  /** Returns the value of the object pointed by the pointer. */
  def ptrValue(ptr: VPtr[Tag]): V

  def ptrValueP(ptr: VPtr[Tag])(implicit p: Primitive[V]): Long

}

trait Updatable[V] extends Values[V] {

  def ptrUpdate(ptr: VPtr[Tag], v: V): Unit

  def ptrUpdateP(ptr: VPtr[Tag], v: Long)(implicit p: Primitive[V]): Unit

}
