package metal

/** Trait for collections that have keys. */
trait Keys[K] extends Pointable {

  type Cap <: Keys[K]

  /** Returns the key pointed by `ptr`. */
  def ptrKey[@specialized L](ptr: MyVPtr): L

}
