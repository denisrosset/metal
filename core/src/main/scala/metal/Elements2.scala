package metal

/** Trait defining a container's enumeration elements. */
trait Elements2[E1, E2] extends Pointable { self =>

  type Cap <: Elements2[E1, E2]
  
  def ptrElement1[@specialized F](ptr: MyVPtr): F

  def ptrElement2[@specialized F](ptr: MyVPtr): F

}
