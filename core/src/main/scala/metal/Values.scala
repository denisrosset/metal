package metal

trait Values[V] extends Pointable {

  /** Returns the value of the object pointed by the pointer. */
  def ptrValue[@specialized W](ptr: VPtr[Tag]): W

}

trait Updatable[V] extends Values[V] {

  def ptrUpdate[@specialized W](ptr: VPtr[Tag], v: W): Unit

  def ptrUpdateFromArray(ptr: VPtr[Tag], array: Array[_], i: Int): Unit = array match {
    case a: Array[Double] => ptrUpdate[Double](ptr, a(i))
    case a: Array[Float] => ptrUpdate[Float](ptr, a(i))
    case a: Array[Long] => ptrUpdate[Long](ptr, a(i))
    case a: Array[Int] => ptrUpdate[Int](ptr, a(i))
    case a: Array[Short] => ptrUpdate[Short](ptr, a(i))
    case a: Array[Byte] => ptrUpdate[Byte](ptr, a(i))
    case a: Array[Boolean] => ptrUpdate[Boolean](ptr, a(i))
    case a: Array[Char] => ptrUpdate[Char](ptr, a(i))
    case a: Array[Unit] => ptrUpdate[Unit](ptr, a(i))
    case a: Array[AnyRef] => ptrUpdate[AnyRef](ptr, a(i))
  }

}
