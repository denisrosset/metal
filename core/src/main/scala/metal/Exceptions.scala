package metal

case class PtrCollOverflowError(n: Int) extends Exception("size %s exceeds max" format n)
class KeyNotFoundException(k: String) extends Exception("key %s was not found" format k)
