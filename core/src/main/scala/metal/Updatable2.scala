package metal

trait Updatable2[V2] extends Values2[V2] {

  def ptrUpdate2[@specialized W2](ptr: VPtr[Tag], v: W2): Unit

  def ptrUpdate2FromArray(ptr: VPtr[Tag], array: Array[_], i: Int): Unit = array match {
    case a: Array[Double] => ptrUpdate2[Double](ptr, a(i))
    case a: Array[Float] => ptrUpdate2[Float](ptr, a(i))
    case a: Array[Long] => ptrUpdate2[Long](ptr, a(i))
    case a: Array[Int] => ptrUpdate2[Int](ptr, a(i))
    case a: Array[Short] => ptrUpdate2[Short](ptr, a(i))
    case a: Array[Byte] => ptrUpdate2[Byte](ptr, a(i))
    case a: Array[Boolean] => ptrUpdate2[Boolean](ptr, a(i))
    case a: Array[Char] => ptrUpdate2[Char](ptr, a(i))
    case a: Array[Unit] => ptrUpdate2[Unit](ptr, a(i))
    case a: Array[AnyRef] => ptrUpdate2[AnyRef](ptr, a(i))
  }

}
