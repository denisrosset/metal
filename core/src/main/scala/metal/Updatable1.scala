package metal

trait Updatable1[V1] extends Values1[V1] {

  type Cap <: Updatable1[V1]

  def ptrUpdate1[@specialized W1](ptr: MyVPtr, v: W1): Unit

  def ptrUpdate1FromArray(ptr: MyVPtr, array: Array[_], i: Int): Unit = array match {
    case a: Array[Double] => ptrUpdate1[Double](ptr, a.asInstanceOf[Array[Double]](i))
    case a: Array[Float] => ptrUpdate1[Float](ptr, a.asInstanceOf[Array[Float]](i))
    case a: Array[Long] => ptrUpdate1[Long](ptr, a.asInstanceOf[Array[Long]](i))
    case a: Array[Int] => ptrUpdate1[Int](ptr, a.asInstanceOf[Array[Int]](i))
    case a: Array[Short] => ptrUpdate1[Short](ptr, a.asInstanceOf[Array[Short]](i))
    case a: Array[Byte] => ptrUpdate1[Byte](ptr, a.asInstanceOf[Array[Byte]](i))
    case a: Array[Boolean] => ptrUpdate1[Boolean](ptr, a.asInstanceOf[Array[Boolean]](i))
    case a: Array[Char] => ptrUpdate1[Char](ptr, a.asInstanceOf[Array[Char]](i))
    case a: Array[Unit] => ptrUpdate1[Unit](ptr, a(i))
    case a: Array[AnyRef] => ptrUpdate1[AnyRef](ptr, a(i))
  }

}
