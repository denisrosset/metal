package metal

/** Trait defining a container's enumeration element. */
trait Elements1[E] extends Pointable { self =>

  type Cap <: Elements1[E]
  
  def ptrElement1[@specialized F](ptr: MyVPtr): F

}
