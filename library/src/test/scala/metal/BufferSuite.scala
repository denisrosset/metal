package metal

class BufferSuite extends MetalSuite {

  test("Buffer is correctly grown even if initially empty") {
    val b1 = metal.mutable.Buffer.empty[Int]
    val b2 = metal.mutable.Buffer[Int]()
    b1 += 1
    b2 += 1
  }

}
