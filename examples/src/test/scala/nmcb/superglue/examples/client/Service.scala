package nmcb
package superglue
package examples
package client

import database.*

case class Service(name: Name, uriPath: UriPath, mock: Json):

  // allow temporary access to repositories
  def assertCorrectInputParameters(inputParameters: Map[Name, Value]): Unit =
    import DataType.*
    import Multiplicity.*
    val requiredParameters     = InputParameterRepository.findByServiceName(name)
    val requiredParameterNames = requiredParameters.map(_.name)
    val requiredAirNames       = requiredParameterNames.flatMap(AirNameRepository.findShallow)
    assert(
      (requiredParameterNames diff inputParameters.keySet).isEmpty

      , s"service $name called with insufficient parameter names: ${inputParameters.keySet}, required=$requiredParameterNames"
    )
    assert(
      requiredParameterNames.forall: name =>
        inputParameters(name) match
          case Value.Text(value)     => requiredParameters.find(p => p.name == name).forall(_.multiplicity == One)
          case Value.Number(value)   => requiredParameters.find(p => p.name == name).forall(_.multiplicity == One)
          case Value.Texts(values)   => requiredParameters.find(p => p.name == name).forall(_.multiplicity == Many)
          case Value.Numbers(values) => requiredParameters.find(p => p.name == name).forall(_.multiplicity == Many)

      , s"service $name called with wrong multiplicity: $inputParameters, required=$requiredParameters"
    )
    assert(
      requiredParameterNames.forall: name =>
        inputParameters(name) match
          case Value.Text(value)     => requiredAirNames.find(p => p.name == name).forall(_.dataType == TextType)
          case Value.Number(value)   => requiredAirNames.find(p => p.name == name).forall(_.dataType == NumberType)
          case Value.Texts(values)   => requiredAirNames.find(p => p.name == name).forall(_.dataType == TextType)
          case Value.Numbers(values) => requiredAirNames.find(p => p.name == name).forall(_.dataType == NumberType)

      , s"service $name called with wrong data type: $inputParameters, required=$requiredAirNames"
    )

  def call(inputParameters: Map[Name,Value]): Json = {
    assert(this.uriPath == uriPath, s"No uri with path $uriPath mocked")
    assertCorrectInputParameters(inputParameters)
    mock
  }

object Service:
  def byUriPath(uriPath: UriPath): Service =
    all.find(_.uriPath == uriPath).getOrElse(sys.error(s"No service on uriPath $uriPath mocked"))

  val all: Set[Service] = Set(

    // TC1 - Generic Test case
    Service(name = "tc-1x1", uriPath = "http://x1/a", mock =
      """{
        |   "result": "text"
        |}
        |""".stripMargin),
    Service(name = "tc1-x2", uriPath = "http://x2/b", mock =
      """{
        |   "result": [101, 102]
        |}
        |""".stripMargin),
    Service(name = "tc1-y1", uriPath = "http://y1/c", mock =
      """{
        |   "nValue": 103
        |}
        |""".stripMargin),
    Service(name = "tc1-y2", uriPath = "http://y2/d", mock =
      """{
        |   "result": 104
        |}
        |""".stripMargin),
    Service(name = "tc1-z1", uriPath = "http://z1/e", mock =
      """{
        |   "result": 105
        |}
        |""".stripMargin),
    Service(name = "tc1-q1", uriPath = "http://q1/m", mock =
      """{
        |   "result": [200, 201, 203]
        |}
        |""".stripMargin),
    Service(name = "tc1-q2", uriPath = "http://q2/n", mock =
      """{
        |   "result": "nValue"
        |}
        |""".stripMargin),
    Service(name = "tc1-r1", uriPath = "http://r1/o", mock =
      """{
        |   "result": 300
        |}
        |""".stripMargin),
    Service(name = "tc1-r2", uriPath = "http://r2/p", mock =
      """{
        |   "result": 400
        |}
        |""".stripMargin),

    // TC2 - Test not Unresolvable
    Service(name = "tc2-s1", uriPath = "http://s1/ab", mock =
      """{
        |   "a": "av",
        |   "cv": "bv"
        |}
        |""".stripMargin),
    Service(name = "tc2-s2", uriPath = "http://s2/c", mock =
      """{
        |   "c": "cv"
        |}
        |""".stripMargin),
    Service(name = "tc2-s3", uriPath = "http://s3/d", mock =
      """{
        |   "d": "dv"
        |}
        |""".stripMargin),

    // TC3 - one to many
    Service(name = "tc3-s1", uriPath = "http://s1/a", mock =
      """{
        |   "a": "av"
        |}
        |""".stripMargin),
    Service(name = "tc3-s2", uriPath = "http://s2/b", mock =
      """{
        |   "b": "bv"
        |}
        |""".stripMargin),
    
    // TC Real World
    Service(name = "rio", uriPath = "http://rio/", mock =
      """{
        |   "ois": [
        |     {
        |       "code": "00AA",
        |       "vs": [{ "vc": "00AA01", "hv": 0 }, { "vc": "00AA02", "hv": 1}],
        |       "dib": "2026-01-01"
        |     },
        |     {
        |       "code": "00AB",
        |       "vs": [{ "vc": "00AB01", "hv": 1 }, { "vc": "00AB02", "hv": 0}],
        |       "dib": "2026-01-02"
        |     }
        |   ]
        |}
        |""".stripMargin),
    Service(name = "owd", uriPath = "http://owd/", mock =
      """{
        |   "ois": [
        |     {
        |       "code": "00AA",
        |       "vs": [{ "vc": "00AA01", "al": 200 }, { "vc": "00AA02", "al": 2}],
        |       "al": 202
        |     },
        |     {
        |       "code": "00AB",
        |       "vs": [{ "vc": "00AB01", "al": 100 }, { "vc": "00AB02", "al": 1}],
        |       "al": 101
        |     }
        |   ]
        |}
        |""".stripMargin)
  )