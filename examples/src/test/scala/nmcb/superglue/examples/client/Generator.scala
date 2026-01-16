package nmcb
package superglue
package examples
package client


object Generator extends App:

  extension (json: Json)
    
    def resolve(path: JsonPath, dataType: DataType, multiplicity: Multiplicity): Value =
      import DataType.*
      import Multiplicity.*
      import Value.*

      import com.jayway.jsonpath
      import jsonpath.spi.json.JacksonJsonProvider
      import jsonpath.*

      import scala.jdk.CollectionConverters.*

      val configuration: Configuration =
        Configuration
          .defaultConfiguration()
          .jsonProvider(new JacksonJsonProvider())
          .addOptions(Option.ALWAYS_RETURN_LIST)

      val array: java.util.LinkedList[?] = jsonpath.JsonPath.using(configuration).parse(json).read(path)
      val result: List[String] = array.asScala.toList.map(_.toString)

      (dataType, multiplicity) match
        case (TextType, One)    => Text(result.head)
        case (NumberType, One)  => Number(result.head.toInt)
        case (TextType, Many)   => Texts(result)
        case (NumberType, Many) => Numbers(result.map(_.toInt))


  def resolve(request: Set[Name], trigger: Map[Name,Value], debug: Boolean = false): Map[Name,Value] =
    import ResolveMethod.*

    Sequencer.sequence(request, trigger.keySet) match
      case Left(error)     => sys.error(error.toString)
      case Right(sequence) =>
        sequence
          .foldLeft(trigger): (result, method) =>
            method match
              case ResolveByTriggerInput(name: Name) =>
                val value = trigger.getOrElse(name, sys.error(s"Unresolved trigger value for: $name"))
                result + (name -> value)
              case ResolveByDeliveryService(name: Name, dataType: DataType, multiplicity: Multiplicity, uriPath: UriPath, jsonPath: JsonPath, parameters: Map[Name,Multiplicity]) =>
                assert((parameters.keySet diff result.keySet).isEmpty, s"Insufficient parameters to call $uriPath required $parameters, resolved ${result.toJson}")
                if debug then println(s"resolving name=$name calling $uriPath")
                val json = Service.byUriPath(uriPath).call(result)
                if debug then println(s"returned json=\n$json")
                
                if debug then println(s"resolving jsonPath=$jsonPath")
                val resolvedJsonPath = jsonPath.replaceAllParameters(result)
                if debug then println(s"returned jsonPath=$resolvedJsonPath")
                
                val value = json.resolve(resolvedJsonPath, dataType, multiplicity)
                if debug then println(s"resolved value=$value")
                
                result + (name -> value)
          .filter((n, v) => request.contains(n))
