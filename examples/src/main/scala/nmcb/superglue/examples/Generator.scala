package nmcb
package superglue
package examples

import com.jayway.jsonpath.spi.json.JacksonJsonProvider

object Generator extends App:
  import service.*
  import delivery.*

  import Value.*

  extension (json: Json)
    def resolve(path: JsonPath, dataType: DataType, multiplicity: Multiplicity): Value =
      import scala.jdk.CollectionConverters.*
      import Value.*
      import DataType.*
      import Multiplicity.*

      import com.jayway.jsonpath
      import jsonpath.Configuration
      import jsonpath.Option

      val configuration: Configuration =
        Configuration
          .defaultConfiguration()
          .jsonProvider(new JacksonJsonProvider())
          .addOptions(
            Option.ALWAYS_RETURN_LIST,
          )

      val array: java.util.LinkedList[?] = jsonpath.JsonPath.using(configuration).parse(json).read(path)
      val result: List[?] = array.asScala.toList.map(_.toString)
      println(s"result=$result (size=${result.size}) found for jsonPath=$path dataType=$dataType, multiplicity=$multiplicity on json=\n$json")
      println(s"result=$result")

      (dataType, multiplicity) match
        case (TEXT, One)    => Text(result.head.toString)
        case (NUMBER, One)  => Number(result.head.toString.toInt)
        case (TEXT, Many)   => Texts(result.map(_.toString))
        case (NUMBER, Many) => Numbers(result.map(_.toString.toInt))


  def resolve(request: Set[Name], trigger: Map[Name,Value]): Map[Name,Value] =
    import ResolveMethod.*
    sequencer.sequence(request, trigger.keySet) match
      case Left(error)     => sys.error(error.toString)
      case Right(sequence) =>
        sequence
          .foldLeft(trigger): (result, method) =>
            method match
              case ResolveByTriggerInput(name: Name) =>
                val value = trigger.getOrElse(name, sys.error(s"Unresolved trigger value for: $name"))
                result + (name -> value)
              case ResolveByDeliverServiceCall(name: Name, dataType: DataType, multiplicity: Multiplicity, uriPath: UriPath, jsonPath: JsonPath, parameters: Set[Name]) =>
                val parms = result.filter((name, value) => parameters.contains(name))
                assert(parms.size == parameters.size, s"Insufficient parameters to call $uriPath required $parameters, resolved ${parms.toJson}")
                val json = Service.byName(uriPath).call(result)
                println(s"calling jsonPath=$jsonPath on json=$json")
                val value = json.resolve(jsonPath, dataType, multiplicity)
                result + (name -> value)
          .filter((n, v) => request.contains(n))


  val trigger = Map("q" -> Number(666))
  val request = Set("a", "b", "c")

  println(resolve(request, trigger).toJson)

