package nmcb
package superglue
package examples

import org.scalatest.funsuite.AnyFunSuite

import Value.*

class GeneratorTest extends AnyFunSuite:
  test("resolve"):
    val trigger = Map("q" -> Number(666))
    val request = Set("a", "b", "c")
    assertResult("""{"a":100,"b":[101,102],"c":103}""")(Generator.resolve(request, trigger).toJson)

