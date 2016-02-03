package metal

/** Trait defining a container's enumeration elements. */
trait Elements2[E2] extends Pointable {

  def ptrElement2[@specialized F](ptr: VPtr[Elements2.this.type]): F

}
