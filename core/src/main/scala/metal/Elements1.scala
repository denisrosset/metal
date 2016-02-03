package metal

/** Trait defining a container's enumeration element. */
trait Elements1[E1] extends Pointable {

  def ptrElement1[@specialized F](ptr: VPtr[Elements1.this.type]): F

}
