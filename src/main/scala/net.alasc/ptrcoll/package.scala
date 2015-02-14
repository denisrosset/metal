package net.alasc

package object ptrcoll {
  type RawPtr = Long
  type TaggedPtr[T] = RawPtr with ({ type Tag = T })
}

