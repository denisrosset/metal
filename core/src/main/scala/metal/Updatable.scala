package metal

trait Updatable[V] extends Values[V] {

  def ptrUpdate[@specialized W](ptr: VPtr[Updatable.this.type], v: W): Unit

  def ptrUpdateFromArray(ptr: VPtr[Updatable.this.type], array: Array[_], i: Int): Unit =
    array match {
      case a: Array[Double] => ptrUpdate[Double](ptr, a.asInstanceOf[Array[Double]](i))
      case a: Array[Float] => ptrUpdate[Float](ptr, a.asInstanceOf[Array[Float]](i))
      case a: Array[Long] => ptrUpdate[Long](ptr, a.asInstanceOf[Array[Long]](i))
      case a: Array[Int] => ptrUpdate[Int](ptr, a.asInstanceOf[Array[Int]](i))
      case a: Array[Short] => ptrUpdate[Short](ptr, a.asInstanceOf[Array[Short]](i))
      case a: Array[Byte] => ptrUpdate[Byte](ptr, a.asInstanceOf[Array[Byte]](i))
      case a: Array[Boolean] => ptrUpdate[Boolean](ptr, a.asInstanceOf[Array[Boolean]](i))
      case a: Array[Char] => ptrUpdate[Char](ptr, a.asInstanceOf[Array[Char]](i))
      case a: Array[Unit] => ptrUpdate[Unit](ptr, a.asInstanceOf[Array[Unit]](i))
      case a: Array[AnyRef] => ptrUpdate[AnyRef](ptr, a(i))
    }

}
