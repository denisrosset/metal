package net.alasc.ptrcoll

import scala.{specialized => sp}

trait Dummy[@sp(Int) A]

object Dummy {
  implicit def fakeInstance[@sp(Int) A]: Dummy[A] = null
}
