package metal.mutable

trait Collection extends metal.Collection {

  /** Invalidates this mutable collection and returns an immutable version of it. */
  def result(): Immutable

  /** Removes all elements from this collection, deallocating its arrays. */
  def clear(): Unit

  /** Removes all elements from this collection, keeping the all allocated arrays. */
  def reset(): Unit
  
}
