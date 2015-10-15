package metal

trait Updatable1[V1] extends Values1[V1] {

  def ptrUpdate1[@specialized W1](ptr: VPtr[Tag], v: W1): Unit

  def ptrUpdate1FromArray(ptr: VPtr[Tag], array: Array[_], i: Int): Unit = array match {
    case a: Array[Double] => ptrUpdate1[Double](ptr, a(i))
    case a: Array[Float] => ptrUpdate1[Float](ptr, a(i))
    case a: Array[Long] => ptrUpdate1[Long](ptr, a(i))
    case a: Array[Int] => ptrUpdate1[Int](ptr, a(i))
    case a: Array[Short] => ptrUpdate1[Short](ptr, a(i))
    case a: Array[Byte] => ptrUpdate1[Byte](ptr, a(i))
    case a: Array[Boolean] => ptrUpdate1[Boolean](ptr, a(i))
    case a: Array[Char] => ptrUpdate1[Char](ptr, a(i))
    case a: Array[Unit] => ptrUpdate1[Unit](ptr, a(i))
    case a: Array[AnyRef] => ptrUpdate1[AnyRef](ptr, a(i))
  }

}
