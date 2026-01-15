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
      val result: List[String] = array.asScala.toList.map(_.toString)

      (dataType, multiplicity) match
        case (TEXT, One)    => Text(result.head)
        case (NUMBER, One)  => Number(result.head.toInt)
        case (TEXT, Many)   => Texts(result)
        case (NUMBER, Many) => Numbers(result.map(_.toInt))


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
                assert((parameters diff result.keySet).isEmpty, s"Insufficient parameters to call $uriPath required $parameters, resolved ${result.toJson}")
                val json = Service.byName(uriPath).call(result)
                val resolvedJsonPath = jsonPath.resolveParameterNames(result)
                val value = json.resolve(resolvedJsonPath, dataType, multiplicity)
                result + (name -> value)
          .filter((n, v) => request.contains(n))
