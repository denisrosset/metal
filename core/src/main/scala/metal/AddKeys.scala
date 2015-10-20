package metal

trait AddKeys[K] extends Keys[K] {

  /** Adds the given key to the collection. If the key is already present, returns the pointer
    * associated with the existing key.
    * If the key is new, any eventual associated values are undefined (they should immediately
    * be updated).
    */
  def ptrAddKey[@specialized L](key: L): VPtr[Tag]

  def ptrAddKeyFromArray(array: Array[_], i: Int): VPtr[Tag] = array match {
    // the additional type cast is needed to avoid boxing
    case a: Array[Double] => ptrAddKey[Double](a.asInstanceOf[Array[Double]](i))
    case a: Array[Float] => ptrAddKey[Float](a.asInstanceOf[Array[Float]](i))
    case a: Array[Long] => ptrAddKey[Long](a.asInstanceOf[Array[Long]](i))
    case a: Array[Int] => ptrAddKey[Int](a.asInstanceOf[Array[Int]](i))
    case a: Array[Short] => ptrAddKey[Short](a.asInstanceOf[Array[Short]](i))
    case a: Array[Byte] => ptrAddKey[Byte](a.asInstanceOf[Array[Byte]](i))
    case a: Array[Boolean] => ptrAddKey[Boolean](a.asInstanceOf[Array[Boolean]](i))
    case a: Array[Char] => ptrAddKey[Char](a.asInstanceOf[Array[Char]](i))
    case a: Array[Unit] => ptrAddKey[Unit](a(i))
    case a: Array[AnyRef] => ptrAddKey[AnyRef](a(i))
  }

}