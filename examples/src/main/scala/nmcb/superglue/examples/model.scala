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


extension (d: Date)
  def toLocalDate: LocalDate = LocalDate.parse(d)

enum Multiplicity:
  case One, Many


enum DataType:
  case TextType, NumberType


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
  case ResolveByDeliveryService(airName: Name, dataType: DataType, multiplicity: Multiplicity, uriPath: UriPath, jsonPath: JsonPath, inputParameters: Map[Name, Multiplicity])


enum Error:
  case UnresolvableCyclicDependency(dependencies: Set[Name])
  case UnresolvableUndefinedDependency(name: Name)

