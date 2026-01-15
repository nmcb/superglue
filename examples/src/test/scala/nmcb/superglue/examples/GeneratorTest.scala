package nmcb
package superglue
package examples

import org.scalatest.funsuite.AnyFunSuite

import Value.*

class GeneratorTest extends AnyFunSuite:

  test("resolve"):
    val trigger = Map("t-oics" -> Numbers(List(666, 999)))
    val request = Set("a", "b", "c")
    val result  = Generator.resolve(request, trigger).toJson
    assertResult("""{"a":"text","b":[101,102],"c":103}""")(result)
