package metal

trait Searchable[K] extends Keys[K] {

  /** Returns a pointer to the given item, if it exists, or null. */
  def ptrFind[@specialized L](key: L): Ptr[Tag]

  def ptrFindFromArray(array: Array[_], i: Int): Ptr[Tag] = array match {
    // the additional type cast is needed to avoid boxing
    case a: Array[Double] => ptrFind[Double](a.asInstanceOf[Array[Double]](i))
    case a: Array[Float] => ptrFind[Float](a.asInstanceOf[Array[Float]](i))
    case a: Array[Long] => ptrFind[Long](a.asInstanceOf[Array[Long]](i))
    case a: Array[Int] => ptrFind[Int](a.asInstanceOf[Array[Int]](i))
    case a: Array[Short] => ptrFind[Short](a.asInstanceOf[Array[Short]](i))
    case a: Array[Byte] => ptrFind[Byte](a.asInstanceOf[Array[Byte]](i))
    case a: Array[Boolean] => ptrFind[Boolean](a.asInstanceOf[Array[Boolean]](i))
    case a: Array[Char] => ptrFind[Char](a.asInstanceOf[Array[Char]](i))
    case a: Array[Unit] => ptrFind[Unit](a(i))
    case a: Array[AnyRef] => ptrFind[AnyRef](a(i))
  }

}
