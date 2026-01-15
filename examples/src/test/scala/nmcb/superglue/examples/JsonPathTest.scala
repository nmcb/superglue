package nmcb.superglue.examples

import nmcb.superglue.examples.Value.*
import org.scalatest.funsuite.AnyFunSuite

class JsonPathTest extends AnyFunSuite:

  test("parameterNames"):
    assertResult(Set("airName-1", "airName_2"))("$.{airName-1}.book[?(@.price < {airName_2})]".parameterNames)

  test("hasParameterNames"):
    assertResult(true)("$.{airName-1}.book[?(@.price < {airName_2})]".hasParameterNames)
    assertResult(false)("$.library.book[?(@.price < 10)]".hasParameterNames)

  test("replaceAllParameters"):
    val parms = Map[Name,Value]("airName-1" -> Text("library"), "airName_2" -> Number(10))
    assertResult("$.library.book[?(@.price < 10)]")("$.{airName-1}.book[?(@.price < {airName_2})]".replaceAllParameters(parms))
