package metal

/** Trait defining a container's enumeration elements. */
trait Elements3[E1, E2, E3] extends Pointable { self =>

  type Cap <: Elements3[E1, E2, E3]
  
  def ptrElement1[@specialized F](ptr: MyVPtr): F

  def ptrElement2[@specialized F](ptr: MyVPtr): F

  def ptrElement3[@specialized F](ptr: MyVPtr): F

}
