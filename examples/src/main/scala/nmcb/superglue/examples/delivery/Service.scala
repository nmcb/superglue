package nmcb
package superglue
package examples
package delivery

import nmcb.superglue.examples.database.InputParameterRepository

case class Service(name: Name, uriPath: UriPath, mock: Json):

  // TODO add assert method, allow tempory access to repositories
  def assertCorrectInputParameters(inputParameters: Map[Name, Value]): Unit =
    import Value.*
    import Multiplicity.*
    val requiredParameters     = InputParameterRepository.findByServiceName(name)
    val requiredParameterNames = requiredParameters.map(_.name)
    assert(
      (requiredParameterNames diff inputParameters.keySet).isEmpty,
      s"service called with insufficient parameter names: ${inputParameters.keySet}, required=$requiredParameterNames"
    )
    assert(
      requiredParameterNames.forall: name =>
        inputParameters(name) match
          case Text(value)     => requiredParameters.find(p => p.name == name).forall(_.multiplicity == One)
          case Number(value)   => requiredParameters.find(p => p.name == name).forall(_.multiplicity == One)
          case Texts(values)   => requiredParameters.find(p => p.name == name).forall(_.multiplicity == Many)
          case Numbers(values) => requiredParameters.find(p => p.name == name).forall(_.multiplicity == Many),
      s"service called with wrong parameters: ${inputParameters}, required=$requiredParameters"
    )

  def call(inputParameters: Map[Name,Value]): Json = {
    assert(this.uriPath == uriPath, s"No uri with path $uriPath mocked")
    assertCorrectInputParameters(inputParameters)
    mock
  }

object Service:
  def byName(uriPath: UriPath): Service =
    all.find(_.uriPath == uriPath).getOrElse(sys.error(s"No service on uriPath $uriPath mocked"))

  val all: Set[Service] = Set(
    Service(name = "x1", uriPath = "http://x1/a", mock =
      """{
        |   "result": "text"
        |}
        |""".stripMargin),
    Service(name = "x2", uriPath = "http://x2/b", mock =
      """{
        |   "result": [101, 102]
        |}
        |""".stripMargin),


    // nValue is used to resolve input parameter c via its json path.
    Service(name = "y1", uriPath = "http://y1/c", mock =
      """{
        |   "nValue": 103
        |}
        |""".stripMargin),
    Service(name = "y2", uriPath = "http://y2/d", mock =
      """{
        |   "result": 104
        |}
        |""".stripMargin),
    Service(name = "z1", uriPath = "http://z1/e", mock =
      """{
        |   "result": 105
        |}
        |""".stripMargin),
    Service(name = "q1", uriPath = "http://q1/m", mock =
      """{
        |   "result": [200, 201, 203]
        |}
        |""".stripMargin),

    // nValue is used to resolve input parameter c via its json path.
    Service(name = "q2", uriPath = "http://q2/n", mock =
      """{
        |   "result": "nValue"
        |}
        |""".stripMargin),
    Service(name = "r1", uriPath = "http://r1/o", mock =
      """{
        |   "result": 300
        |}
        |""".stripMargin),
    Service(name = "r2", uriPath = "http://r2/p", mock =
      """{
        |   "result": 400
        |}
        |""".stripMargin),

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