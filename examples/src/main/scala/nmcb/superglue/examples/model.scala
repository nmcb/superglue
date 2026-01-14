package nmcb
package superglue
package examples

import java.time.LocalDate
import scala.util.Try

type Name     = String
type UriPath  = String
type Json     = String
type JsonPath = String
type Date     = String

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

extension (nv: (Name, Value))
  def toJson: Json = {
    import Value.*
    nv match
      case (name, Number(value))   => s"\"$name\":$value"
      case (name, Text(value))     => s"\"$name\":\"$value\""
      case (name, Numbers(values)) => s"\"$name\":[${values.mkString(",")}]"
      case (name, Texts(values))   => s"\"$name\":[${values.map(v => s"\"v\"").mkString(",")}]"
  }

extension (parameters: Map[Name, Value])
  def toJson: Json =
    parameters.map(_.toJson).mkString("{", ",", "}")
