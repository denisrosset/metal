package metal

import org.scalatest.{FunSuite, PropSpec, Matchers}
import org.scalatest.prop.{Configuration, GeneratorDrivenPropertyChecks}

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary

import org.scalactic.anyvals.{PosZDouble, PosInt}

trait MetalSuite extends FunSuite with Matchers with GeneratorDrivenPropertyChecks
