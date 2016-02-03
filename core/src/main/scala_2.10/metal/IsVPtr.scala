package metal

object IsVPtr {

  @inline final def unapply[C <: Pointable with Singleton](ptr: Ptr[C]): Option[VPtr[C]] =
    if (ptr.isEmpty) None else Some(ptr.get)

}
