package net.alasc.ptrcoll

import scala.{specialized => sp}

class Dummy[@sp A]

object Dummy {
  implicit def nullInstance[@sp A]: Dummy[A] = null
}

class Dummy2[@sp A, @sp B]

object Dummy2 {
  implicit def nullInstance[@sp A, @sp B]: Dummy2[A, B] = null
}
