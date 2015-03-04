package net.alasc

package object ptrcoll {
  /** Raw primitive pointer type = Long (8 bytes). */
  type RawPtr = Long
  /** Tagged pointer, with `T` an inner type of a collection. */ 
  type TaggedPtr[T] = RawPtr with ({ type Tag = T })
  type ValidTaggedPtr[T] = TaggedPtr[T] with ({ type Valid = Unit })
}
