package metal

class Dummy[A]

object Dummy {
  implicit def nullInstance[A]: Dummy[A] = null
}

class Dummy2[A, B]

object Dummy2 {
  implicit def nullInstance[A, B]: Dummy2[A, B] = null
}
