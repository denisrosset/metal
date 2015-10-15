package metal

class Dummy2[@specialized A, @specialized B]

object Dummy2 {
  implicit def apply[@specialized A, @specialized B]: Dummy2[A, B] = null
}
