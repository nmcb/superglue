package nmcb
package superglue
package examples

import org.scalatest.funsuite.AnyFunSuite

import client.*
import Value.*

class SequencerTest extends AnyFunSuite:

  test("tc1"):
    val trigger = Map("tc1-q" -> Texts(List("00AA", "00AB")))
    val request = Set("tc1-a", "tc1-b", "tc1-c")
    val result  = Generator.resolve(request, trigger).toJson
    assertResult("""{"tc1-a":"text","tc1-b":[101,102],"tc1-c":103}""")(result)

  test("tc2"):
    val trigger = Map("tc2-q" -> Text("ignored"))
    val request = Set("tc2-d")
    val result = Generator.resolve(request, trigger).toJson
    assertResult("""{"tc2-d":"dv"}""")(result)
