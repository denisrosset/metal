package metal

import org.scalatest.{FunSuite, Matchers}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks}

trait MetalSuite extends FunSuite with Matchers with GeneratorDrivenPropertyChecks
