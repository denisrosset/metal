package net.alasc.ptrcoll

trait Pointable {
  /** Tag of this instance, used to select the correct typeclass for
    * the pointer syntax. */
  trait Tag
}
