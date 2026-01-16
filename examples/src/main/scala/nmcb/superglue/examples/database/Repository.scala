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

    // TC1
    AirNameEntity(name = "tc1-a", deliveryMethod = DeliverServiceType, dataType = TextType),
    AirNameEntity(name = "tc1-b", deliveryMethod = DeliverServiceType, dataType = NumberType),
    AirNameEntity(name = "tc1-c", deliveryMethod = DeliverServiceType, dataType = NumberType),
    AirNameEntity(name = "tc1-d", deliveryMethod = DeliverServiceType, dataType = NumberType),
    AirNameEntity(name = "tc1-e", deliveryMethod = DeliverServiceType, dataType = NumberType),
    AirNameEntity(name = "tc1-m", deliveryMethod = DeliverServiceType, dataType = NumberType),
    AirNameEntity(name = "tc1-n", deliveryMethod = DeliverServiceType, dataType = TextType),
    AirNameEntity(name = "tc1-o", deliveryMethod = DeliverServiceType, dataType = NumberType),
    AirNameEntity(name = "tc1-p", deliveryMethod = DeliverServiceType, dataType = NumberType),
    AirNameEntity(name = "tc1-q", deliveryMethod = TriggerType, dataType = TextType),

    // TC2
    AirNameEntity(name = "tc2-a", deliveryMethod = DeliverServiceType, dataType = TextType),
    AirNameEntity(name = "tc2-b", deliveryMethod = DeliverServiceType, dataType = TextType),
    AirNameEntity(name = "tc2-c", deliveryMethod = DeliverServiceType, dataType = TextType),
    AirNameEntity(name = "tc2-d", deliveryMethod = DeliverServiceType, dataType = TextType),
    AirNameEntity(name = "tc2-q", deliveryMethod = TriggerType, dataType = TextType),

    // TC3
    AirNameEntity(name = "tc3-a", deliveryMethod = DeliverServiceType, dataType = TextType),
    AirNameEntity(name = "tc3-b", deliveryMethod = DeliverServiceType, dataType = TextType),

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
    // TC1
    DeliverServicePeriodEntity(jsonPath = "$.result", multiplicity = One, fromDate = "2026-01-01", toDate = null, serviceName = "tc1-x1", airName = "tc1-a"),
    DeliverServicePeriodEntity(jsonPath = "$.result.*", multiplicity = Many, fromDate = "2026-01-01", toDate = null, serviceName = "tc1-x2", airName = "tc1-b"),
    DeliverServicePeriodEntity(jsonPath = "$.{tc1-n}", multiplicity = One, fromDate = "2026-01-01", toDate = null, serviceName = "tc1-y1", airName = "tc1-c"),
    DeliverServicePeriodEntity(jsonPath = "$.result", multiplicity = One, fromDate = "2026-01-01", toDate = null, serviceName = "tc1-y2", airName = "tc1-d"),
    DeliverServicePeriodEntity(jsonPath = "$.result", multiplicity = One, fromDate = "2026-01-01", toDate = null, serviceName = "tc1-z1", airName = "tc1-e"),
    DeliverServicePeriodEntity(jsonPath = "$.result.*", multiplicity = Many, fromDate = "2026-01-01", toDate = null, serviceName = "tc1-q1", airName = "tc1-m"),
    DeliverServicePeriodEntity(jsonPath = "$.result", multiplicity = One, fromDate = "2026-01-01", toDate = null, serviceName = "tc1-q2", airName = "tc1-n"),
    DeliverServicePeriodEntity(jsonPath = "$.result", multiplicity = One, fromDate = "2026-01-01", toDate = null, serviceName = "tc1-r1", airName = "tc1-o"),
    DeliverServicePeriodEntity(jsonPath = "$.result", multiplicity = One, fromDate = "2026-01-01", toDate = null, serviceName = "tc1-r2", airName = "tc1-p"),

    // TC2
    DeliverServicePeriodEntity(jsonPath = "$.a", multiplicity = One, fromDate = "2026-01-01", toDate = null, serviceName = "tc2-s1", airName = "tc2-a"),
    DeliverServicePeriodEntity(jsonPath = "$.{tc2-c}", multiplicity = One, fromDate = "2026-01-01", toDate = null, serviceName = "tc2-s1", airName = "tc2-b"),
    DeliverServicePeriodEntity(jsonPath = "$.c", multiplicity = One, fromDate = "2026-01-01", toDate = null, serviceName = "tc2-s2", airName = "tc2-c"),
    DeliverServicePeriodEntity(jsonPath = "$.d", multiplicity = One, fromDate = "2026-01-01", toDate = null, serviceName = "tc2-s3", airName = "tc2-d"),

    // TC3
    DeliverServicePeriodEntity(jsonPath = "$.a", multiplicity = One, fromDate = "2026-01-01", toDate = null, serviceName = "tc3-s1", airName = "tc3-a"),
    DeliverServicePeriodEntity(jsonPath = "$.b", multiplicity = One, fromDate = "2026-01-01", toDate = null, serviceName = "tc3-21", airName = "tc3-b"),

    DeliverServicePeriodEntity(jsonPath = "$.ois[?(@.code == {an-code})].dib", multiplicity = One, fromDate = "2026-01-01", toDate = null, serviceName = "rio", airName = "an-dib"),
  )
  def findByAirName(n: Name): Set[DeliverServicePeriodEntity] =
    shallow.filter(_.airName == n).map: e =>
      e.copy(deliverServiceEntity = DeliverServiceRepository.find(e.serviceName))

object DeliverServiceRepository extends Repository[Name, DeliverServiceEntity]:
  def key(e: DeliverServiceEntity): Name = e.name
  def shallow: Set[DeliverServiceEntity] = Set(
    // TC1
    DeliverServiceEntity(name = "tc1-x1", uriPath = "http://x1/a"),
    DeliverServiceEntity(name = "tc1-x2", uriPath = "http://x2/b"),
    DeliverServiceEntity(name = "tc1-y1", uriPath = "http://y1/c"),
    DeliverServiceEntity(name = "tc1-y2", uriPath = "http://y2/d"),
    DeliverServiceEntity(name = "tc1-z1", uriPath = "http://z1/e"),
    DeliverServiceEntity(name = "tc1-q1", uriPath = "http://q1/m"),
    DeliverServiceEntity(name = "tc1-q2", uriPath = "http://q2/n"),
    DeliverServiceEntity(name = "tc1-r1", uriPath = "http://r1/o"),
    DeliverServiceEntity(name = "tc1-r2", uriPath = "http://r2/p"),

    // TC2
    DeliverServiceEntity(name = "tc2-s1", uriPath = "http://s1/ab"),
    DeliverServiceEntity(name = "tc2-s2", uriPath = "http://s2/c"),
    DeliverServiceEntity(name = "tc2-s3", uriPath = "http://s3/d"),

    DeliverServiceEntity(name = "rio", uriPath = "http://rio/an-dib")

  )
  def find(k: Name): Option[DeliverServiceEntity] =
    findShallow(k).map: e =>
      e.copy(inputParameterEntities = InputParameterRepository.findByServiceName(e.name))

object InputParameterRepository extends Repository[Name, InputParameterEntity]:
  import Multiplicity.*
  def key(e: InputParameterEntity): Name = e.name
  def shallow: Set[InputParameterEntity] = Set(
    // TC1
    InputParameterEntity(name = "tc1-b", serviceName= "tc1-x1", multiplicity = Many, fromDate = "2026-01-01", toDate = null),
    InputParameterEntity(name = "tc1-c", serviceName= "tc1-x1", multiplicity = One, fromDate = "2026-01-01", toDate = null),
    InputParameterEntity(name = "tc1-c", serviceName= "tc1-x2", multiplicity = One, fromDate = "2026-01-01", toDate = null),
    InputParameterEntity(name = "tc1-q", serviceName= "tc1-x1", multiplicity = Many, fromDate = "2026-01-01", toDate = null),
    InputParameterEntity(name = "tc1-q", serviceName= "tc1-y2", multiplicity = Many, fromDate = "2026-01-01", toDate = null),
    InputParameterEntity(name = "tc1-q", serviceName= "tc1-q2", multiplicity = Many, fromDate = "2026-01-01", toDate = null),
    InputParameterEntity(name = "tc1-d", serviceName= "tc1-x2", multiplicity = One, fromDate = "2026-01-01", toDate = null),
    InputParameterEntity(name = "tc1-e", serviceName= "tc1-y1", multiplicity = One, fromDate = "2026-01-01", toDate = null),
    InputParameterEntity(name = "tc1-m", serviceName= "tc1-z1", multiplicity = Many, fromDate = "2026-01-01", toDate = null),
    InputParameterEntity(name = "tc1-d", serviceName= "tc1-z1", multiplicity = One, fromDate = "2026-01-01", toDate = null),
    InputParameterEntity(name = "tc1-n", serviceName= "tc1-q1", multiplicity = One, fromDate = "2026-01-01", toDate = null),
    InputParameterEntity(name = "tc1-p", serviceName= "tc1-r1", multiplicity = One, fromDate = "2026-01-01", toDate = null),
    InputParameterEntity(name = "tc1-o", serviceName= "tc1-r2", multiplicity = One, fromDate = "2026-01-01", toDate = null),

    // TC2
    InputParameterEntity(name = "tc2-q", serviceName= "tc2-s1", multiplicity = One, fromDate = "2026-01-01", toDate = null),
    InputParameterEntity(name = "tc2-a", serviceName= "tc2-s2", multiplicity = One, fromDate = "2026-01-01", toDate = null),
    InputParameterEntity(name = "tc2-b", serviceName= "tc2-s3", multiplicity = One, fromDate = "2026-01-01", toDate = null),
    InputParameterEntity(name = "tc2-c", serviceName= "tc2-s3", multiplicity = One, fromDate = "2026-01-01", toDate = null),

    InputParameterEntity(name = "tc1-q", serviceName= "rio", multiplicity = Many, fromDate = "2026-01-01", toDate = null),

  )
  def findByServiceName(n: Name): Set[InputParameterEntity] =
    shallow.filter(_.serviceName == n)
