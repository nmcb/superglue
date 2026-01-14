package nmcb
package superglue
package examples
package database

case class AirNameEntity(name: Name, dataType: DataType, deliveryMethod: ResolveMethodType, deliveryServicePeriodEntities: Set[DeliverServicePeriodEntity] = Set.empty)

case class DeliverServicePeriodEntity(jsonPath: JsonPath, startDate: Date, endDate: Date = null, serviceName: Name, airName: Name, deliverServiceEntity: Option[DeliverServiceEntity] = None) extends WithPeriod

case class DeliverServiceEntity(name: Name, uriPath: UriPath, inputParameterEntities: Set[InputParameterEntity] = Set.empty)

case class InputParameterEntity(name: Name, serviceName: Name, multiplicity: Multiplicity, startDate: Date, endDate: Date = null) extends WithPeriod

case class CalculationServiceEntity(name: Name)