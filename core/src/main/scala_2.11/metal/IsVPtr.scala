package metal

object IsVPtr {

  // for name-based extraction
  @inline final def unapply[C <: Pointable with Singleton](ptr: Ptr[C]): Ptr[C] = ptr

}
