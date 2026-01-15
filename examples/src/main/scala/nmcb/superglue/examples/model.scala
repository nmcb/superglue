package nmcb
package superglue
package examples

import java.time.LocalDate

type Name     = String
type UriPath  = String
type Json     = String
type JsonPath = String
type Date     = String

extension (path: JsonPath)

  def parameterNames: Set[Name] =
    "\\{([a-zA-Z][a-zA-Z0-9\\-_]*)}"
      .r
      .unanchored
      .findAllMatchIn(path)
      .map(_.group(1))
      .toSet

  def hasParameterNames: Boolean =
    parameterNames.nonEmpty

  def resolveParameterNames(parameters: Map[Name,Value]): JsonPath =
    assert((parameterNames diff parameters.keySet).isEmpty, s"insufficient replacements, given ${parameters.keySet} required $parameterNames")
    val resolved =
      parameters
        .filter: (name, value) =>
          parameterNames.contains(name)
        .foldLeft(path):
          case (result, (name, value)) =>
            result.replace(s"{$name}", value.toJsonPathParameter)
    resolved


extension (d: Date)
  def toLocalDate: LocalDate = LocalDate.parse(d)

enum Multiplicity:
  case One, Many

enum DataType:
  case TEXT, NUMBER

case class Period(startDate: Date, endDate: Date):
  def from: LocalDate = startDate.toLocalDate
  def to: Option[LocalDate] = Option(endDate).map(_.toLocalDate)

trait WithPeriod:
  def fromDate: Date
  def toDate: Date
  def period: Period = Period(fromDate, toDate)

enum ResolveMethodType:
  case DeliverServiceType, CalculationServiceType, TriggerType

enum ResolveMethod:
  case ResolveByTriggerInput(airName: Name)
  case ResolveByDeliverServiceCall(airName: Name, dataType: DataType, multiplicity: Multiplicity, uriPath: UriPath, jsonPath: JsonPath, inputParameters: Set[Name])

enum Error:
  case UnresolvableCyclicDependency(dependencies: Set[Name])
  case UnresolvableUndefinedDependency(name: Name)

case class AirName(name: Name, dataType: DataType, multiplicity: Multiplicity, resolve: Error)

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
    parameters.map(_.toJson).mkString("{", ",", "}")
