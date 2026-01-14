package nmcb
package superglue
package examples
package delivery

case class Service(uriPath: UriPath, mock: Json):
  def call(inputParameters: Map[Name,Value]): Json = {
    assert(this.uriPath == uriPath, s"No uri with path $uriPath mocked")
    println(s"$uriPath called with '${inputParameters.toJson}'")
    mock
  }

object Service:
  def byName(uriPath: UriPath): Service =
    all.find(_.uriPath == uriPath).getOrElse(sys.error(s"No service on uriPath $uriPath mocked"))

  val all: Set[Service] = Set(
    Service(uriPath = "http://x1/a", mock =
      """{
        |   "result": 100
        |}
        |""".stripMargin),
    Service(uriPath = "http://x2/b", mock =
      """{
        |   "result": [101, 102]
        |}
        |""".stripMargin),
    Service(uriPath = "http://y1/c", mock =
      """{
        |   "result": 103
        |}
        |""".stripMargin),
    Service(uriPath = "http://y2/d", mock =
      """{
        |   "result": 104
        |}
        |""".stripMargin),
    Service(uriPath = "http://z1/e", mock =
      """{
        |   "result": 105
        |}
        |""".stripMargin),
    Service(uriPath = "http://q1/m", mock =
      """{
        |   "result": [200, 201, 203]
        |}
        |""".stripMargin),
    Service(uriPath = "http://q2/n", mock =
      """{
        |   "result": 203
        |}
        |""".stripMargin),
    Service(uriPath = "http://r1/o", mock =
      """{
        |   "result": 300
        |}
        |""".stripMargin),
    Service(uriPath = "http://r2/p", mock =
      """{
        |   "result": 400
        |}
        |""".stripMargin)
  )