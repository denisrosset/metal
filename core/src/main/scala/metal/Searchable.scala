package metal

trait Searchable[K] extends Keys[K] {

  /** Returns a pointer to the given item, if it exists, or null. */
  def ptrFind[@specialized L](key: L): Ptr[Tag]

}
