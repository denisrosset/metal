package metal

class Dummy[@specialized A]

object Dummy {
  implicit def apply[@specialized A]: Dummy[A] = null
}
