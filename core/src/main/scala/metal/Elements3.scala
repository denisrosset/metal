package metal

/** Trait defining a container's enumeration elements. */
trait Elements3[E3] extends Pointable {

  def ptrElement3[@specialized F](ptr: VPtr[Elements3.this.type]): F

}
