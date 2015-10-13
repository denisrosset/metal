package metal

trait Searchable[K] extends Keys[K] {

  /** Returns a pointer to the given item, if it exists, or null. */
  def ptrFind(key: K): Ptr[Tag]

  def ptrFindP(key: Long)(implicit p: Primitive[K]): Ptr[Tag]

}
