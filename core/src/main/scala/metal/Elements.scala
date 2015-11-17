package metal

/** Trait defining a container's enumeration element. */
trait Elements[E] extends Pointable { self =>

  type Cap <: Elements[E]
  
  def ptrElement[@specialized F](ptr: MyVPtr): F

}
