package nmcb
package superglue
package examples
package database

case class DoesNotExist(name: Name) extends RuntimeException(s"Air name $name does not exist")

trait Repository[K, E]:
  protected def key(e: E): K
  protected def shallow: Set[E]

  def findShallow(k: K): Option[E] =
    val result = shallow.filter(e => key(e) == k)
    result.size match
      case 1 => result.headOption
      case 0 => None
      case _ => sys.error(s"duplicate key $k")

object AirNameRepository extends Repository[Name, AirNameEntity]:
  import DataType.*
  import ResolveMethodType.*
  def key(e: AirNameEntity): Name = e.name
  def shallow: Set[AirNameEntity] = Set(
    AirNameEntity(name = "a", deliveryMethod = DeliverServiceType, dataType = TextType),
    AirNameEntity(name = "b", deliveryMethod = DeliverServiceType, dataType = NumberType),
    AirNameEntity(name = "c", deliveryMethod = DeliverServiceType, dataType = NumberType),
    AirNameEntity(name = "d", deliveryMethod = DeliverServiceType, dataType = NumberType),
    AirNameEntity(name = "e", deliveryMethod = DeliverServiceType, dataType = NumberType),
    AirNameEntity(name = "m", deliveryMethod = DeliverServiceType, dataType = NumberType),
    AirNameEntity(name = "n", deliveryMethod = DeliverServiceType, dataType = TextType),
    AirNameEntity(name = "o", deliveryMethod = DeliverServiceType, dataType = NumberType),
    AirNameEntity(name = "p", deliveryMethod = DeliverServiceType, dataType = NumberType),
    AirNameEntity(name = "t-oics", deliveryMethod = TriggerType, dataType = TextType),

    AirNameEntity(name = "an-dib", deliveryMethod = DeliverServiceType, dataType = TextType),
    AirNameEntity(name = "an-al", deliveryMethod = DeliverServiceType, dataType = NumberType),
    AirNameEntity(name = "an-code", deliveryMethod = DeliverServiceType, dataType = TextType),
    AirNameEntity(name = "an-vs", deliveryMethod = DeliverServiceType, dataType = TextType),
  )
  def get(n: Name): AirNameEntity =
    findShallow(n)
      .map: e =>
        e.copy(deliveryServicePeriodEntities = DeliveryServicePeriodRepository.findByAirName(e.name))
      .getOrElse(throw DoesNotExist(s"Air name $n does not exist"))

object DeliveryServicePeriodRepository extends Repository[DeliverServicePeriodEntity, DeliverServicePeriodEntity]:
  import Multiplicity.*
  def key(e: DeliverServicePeriodEntity): DeliverServicePeriodEntity = e
  def shallow: Set[DeliverServicePeriodEntity] = Set(
    DeliverServicePeriodEntity(jsonPath = "$.result", multiplicity = One, fromDate = "2026-01-01", toDate = null, serviceName = "x1", airName = "a"),
    DeliverServicePeriodEntity(jsonPath = "$.result.*", multiplicity = Many, fromDate = "2026-01-01", toDate = null, serviceName = "x2", airName = "b"),
    DeliverServicePeriodEntity(jsonPath = "$.{n}", multiplicity = One, fromDate = "2026-01-01", toDate = null, serviceName = "y1", airName = "c"),
    DeliverServicePeriodEntity(jsonPath = "$.result", multiplicity = One, fromDate = "2026-01-01", toDate = null, serviceName = "y2", airName = "d"),
    DeliverServicePeriodEntity(jsonPath = "$.result", multiplicity = One, fromDate = "2026-01-01", toDate = null, serviceName = "z1", airName = "e"),
    DeliverServicePeriodEntity(jsonPath = "$.result.*", multiplicity = Many, fromDate = "2026-01-01", toDate = null, serviceName = "q1", airName = "m"),
    DeliverServicePeriodEntity(jsonPath = "$.result", multiplicity = One, fromDate = "2026-01-01", toDate = null, serviceName = "q2", airName = "n"),
    DeliverServicePeriodEntity(jsonPath = "$.result", multiplicity = One, fromDate = "2026-01-01", toDate = null, serviceName = "r1", airName = "o"),
    DeliverServicePeriodEntity(jsonPath = "$.result", multiplicity = One, fromDate = "2026-01-01", toDate = null, serviceName = "r2", airName = "p"),
    DeliverServicePeriodEntity(jsonPath = "$.ois[?(@.code == {an-code})].dib", multiplicity = One, fromDate = "2026-01-01", toDate = null, serviceName = "rio", airName = "an-dib"),
  )
  def findByAirName(n: Name): Set[DeliverServicePeriodEntity] =
    shallow.filter(_.airName == n).map: e =>
      e.copy(deliverServiceEntity = DeliverServiceRepository.find(e.serviceName))

object DeliverServiceRepository extends Repository[Name, DeliverServiceEntity]:
  def key(e: DeliverServiceEntity): Name = e.name
  def shallow: Set[DeliverServiceEntity] = Set(
    DeliverServiceEntity(name = "x1", uriPath = "http://x1/a"),
    DeliverServiceEntity(name = "x2", uriPath = "http://x2/b"),
    DeliverServiceEntity(name = "y1", uriPath = "http://y1/c"),
    DeliverServiceEntity(name = "y2", uriPath = "http://y2/d"),
    DeliverServiceEntity(name = "z1", uriPath = "http://z1/e"),
    DeliverServiceEntity(name = "q1", uriPath = "http://q1/m"),
    DeliverServiceEntity(name = "q2", uriPath = "http://q2/n"),
    DeliverServiceEntity(name = "r1", uriPath = "http://r1/o"),
    DeliverServiceEntity(name = "r2", uriPath = "http://r2/p"),

    DeliverServiceEntity(name = "rio", uriPath = "http://rio/an-dib")

  )
  def find(k: Name): Option[DeliverServiceEntity] =
    findShallow(k).map: e =>
      e.copy(inputParameterEntities = InputParameterRepository.findByServiceName(e.name))

object InputParameterRepository extends Repository[Name, InputParameterEntity]:
  import Multiplicity.*
  def key(e: InputParameterEntity): Name = e.name
  def shallow: Set[InputParameterEntity] = Set(
    InputParameterEntity(name = "b", serviceName= "x1", multiplicity = Many, fromDate = "2026-01-01", toDate = null),
    InputParameterEntity(name = "c", serviceName= "x1", multiplicity = One, fromDate = "2026-01-01", toDate = null),
    InputParameterEntity(name = "c", serviceName= "x2", multiplicity = One, fromDate = "2026-01-01", toDate = null),
    InputParameterEntity(name = "t-oics", serviceName= "x1", multiplicity = Many, fromDate = "2026-01-01", toDate = null),
    InputParameterEntity(name = "t-oics", serviceName= "y2", multiplicity = Many, fromDate = "2026-01-01", toDate = null),
    InputParameterEntity(name = "t-oics", serviceName= "q2", multiplicity = Many, fromDate = "2026-01-01", toDate = null),
    InputParameterEntity(name = "d", serviceName= "x2", multiplicity = One, fromDate = "2026-01-01", toDate = null),
    InputParameterEntity(name = "e", serviceName= "y1", multiplicity = One, fromDate = "2026-01-01", toDate = null),
    InputParameterEntity(name = "m", serviceName= "z1", multiplicity = Many, fromDate = "2026-01-01", toDate = null),
    InputParameterEntity(name = "d", serviceName= "z1", multiplicity = One, fromDate = "2026-01-01", toDate = null),
    InputParameterEntity(name = "n", serviceName= "q1", multiplicity = One, fromDate = "2026-01-01", toDate = null),
    InputParameterEntity(name = "p", serviceName= "r1", multiplicity = One, fromDate = "2026-01-01", toDate = null),
    InputParameterEntity(name = "o", serviceName= "r2", multiplicity = One, fromDate = "2026-01-01", toDate = null),

    InputParameterEntity(name = "t-oics", serviceName= "rio", multiplicity = Many, fromDate = "2026-01-01", toDate = null),

  )
  def findByServiceName(n: Name): Set[InputParameterEntity] =
    shallow.filter(_.serviceName == n)
