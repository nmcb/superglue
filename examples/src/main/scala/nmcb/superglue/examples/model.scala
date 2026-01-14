package nmcb
package superglue
package examples

import java.time.LocalDate

type Name     = String
type UriPath  = String
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
  def startDate: Date
  def endDate: Date
  def period: Period = Period(startDate, endDate)

enum ResolveMethodType:
  case DeliverServiceType, CalculationServiceType, TriggerType

enum ResolveMethod:
  case ResolveByTriggerInput(airName: Name)
  case ResolveByDeliverServiceCall(airName: Name, uriPath: UriPath, jsonPath: JsonPath, inputParameters: Set[Name])
  
enum Error:
  case UnresolvableCyclicDependency(dependencies: Set[Name])
  case UnresolvableUndefinedDependency(name: Name)  

case class AirName(name: Name, dataType: DataType, multiplicity: Multiplicity, resolve: Error)

type Value = String | List[String]
