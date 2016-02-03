package metal

trait Updatable2[V2] extends Values2[V2] {

  def ptrUpdate2[@specialized W2](ptr: VPtr[Updatable2.this.type], v: W2): Unit

  def ptrUpdate2FromArray(ptr: VPtr[Updatable2.this.type], array: Array[_], i: Int): Unit =
    array match {
      case a: Array[Double] => ptrUpdate2[Double](ptr, a.asInstanceOf[Array[Double]](i))
      case a: Array[Float] => ptrUpdate2[Float](ptr, a.asInstanceOf[Array[Float]](i))
      case a: Array[Long] => ptrUpdate2[Long](ptr, a.asInstanceOf[Array[Long]](i))
      case a: Array[Int] => ptrUpdate2[Int](ptr, a.asInstanceOf[Array[Int]](i))
      case a: Array[Short] => ptrUpdate2[Short](ptr, a.asInstanceOf[Array[Short]](i))
      case a: Array[Byte] => ptrUpdate2[Byte](ptr, a.asInstanceOf[Array[Byte]](i))
      case a: Array[Boolean] => ptrUpdate2[Boolean](ptr, a.asInstanceOf[Array[Boolean]](i))
      case a: Array[Char] => ptrUpdate2[Char](ptr, a.asInstanceOf[Array[Char]](i))
      case a: Array[Unit] => ptrUpdate2[Unit](ptr, a.asInstanceOf[Array[Unit]](i))
      case a: Array[AnyRef] => ptrUpdate2[AnyRef](ptr, a(i))
    }

}
