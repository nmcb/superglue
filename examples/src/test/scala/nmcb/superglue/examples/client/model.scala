package nmcb
package superglue
package examples
package client

enum Value:
  case Text(value: String)
  case Number(value: Int)
  case Texts(values: List[String])
  case Numbers(values: List[Int])

  def toJsonPathParameter: String =
    this match
      case Text(value)     => value
      case Number(value)   => value.toString
      case vs              => sys.error(s"unsupported path multiplicity: $vs")

  def toJson: Json =
    this match
      case Text(value)     => s"\"$value\""
      case Number(value)   => value.toString
      case Texts(values)   => s"[${values.map(v => s"\"v\"").mkString(",")}]"
      case Numbers(values) => s"[${values.mkString(",")}]"


extension (nv: (Name, Value))

  def toJson: Json =
    val (name, value) = nv
    s"\"$name\":${value.toJson}"


extension (parameters: Map[Name, Value])

  def toJson: Json =
    parameters.toVector.sortBy((name, value) => name).map(_.toJson).mkString("{", ",", "}")


extension (path: JsonPath)

  def replaceAllParameters(parameters: Map[Name,Value]): JsonPath =
      val resolved =
        parameters
          .filter: (name, value) =>
            path.parameterNames.contains(name)
          .foldLeft(path):
            case (result, (name, value)) =>
              result.replace(s"{$name}", value.toJsonPathParameter)
      resolved
