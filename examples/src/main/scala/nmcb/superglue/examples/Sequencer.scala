package nmcb
package superglue
package examples

import scala.annotation.tailrec

object Sequencer:

  private type Graph = Map[Name, Set[Name]]

  private def buildGraph(namesToResolve: Set[Name]): Graph =
    @tailrec
    def loop(todo: Set[Name], result: Set[Name] = Set.empty): Set[Name] =
      if todo.isEmpty then
        result
      else
        val found = result ++ todo
        val next  = found.flatMap(dependenciesOf) -- found -- namesToResolve
        loop(next, found)

    loop(namesToResolve).map(n => n -> dependenciesOf(n)).toMap

  extension (graph: Graph)

    private def isCyclicFor(namesToResolve: Set[Name]): Boolean =
      def loop(name: Name, visited: Set[Name] = Set.empty[Name]): Boolean =
        if visited.contains(name) then
          true
        else
          graph.getOrElse(name, Set.empty).exists: dependency =>
            if graph.contains(dependency) then
              loop(dependency, visited + name)
            else
              false
      namesToResolve.exists(r => loop(r))

    private def sequenceFor(namesSuppliedByTrigger: Set[Name]): Vector[Name] =
      @tailrec
      def loop(todo: Map[Name, Set[Name]], result: Vector[Name]): Vector[Name] =
        val (leafs, branches) = todo.partition((name, dependencies) => (dependencies -- namesSuppliedByTrigger).isEmpty)
        if leafs.isEmpty then
          result
        else
          val purged = branches.map((name, dependencies) => name -> (dependencies -- leafs.keySet))
          loop(purged, result :++ leafs.keySet)

      val (leafs, branches) = graph.partition((name, dependencies) => dependencies.isEmpty)
      loop(branches, leafs.keySet.toVector)

  // database access

  import ResolveMethod.*
  import ResolveMethodType.*
  import database.*

  private def dependenciesOf(name: Name): Set[Name] =
    val entity = AirNameRepository.get(name)
    for
      period        <- entity.deliveryServicePeriodEntities
      service       <- period.deliverServiceEntity.toSet
      parameterName <- service.inputParameterEntities.map(_.name) ++ period.jsonPath.parameterNames
    yield
      parameterName

  private def resolveMethodFor(name: Name): ResolveMethod =
    val entity = AirNameRepository.get(name)
    entity.deliveryMethod match
      case DeliverServiceType =>
        val result =
          for
            period    <- entity.deliveryServicePeriodEntities
            service   <- period.deliverServiceEntity.toSet
          yield
            ResolveByDeliveryService(
              airName         = entity.name,
              dataType        = entity.dataType,
              multiplicity    = period.multiplicity,
              uriPath         = service.uriPath,
              jsonPath        = period.jsonPath,
              inputParameters = service.inputParameterEntities.map(p => p.name -> p.multiplicity).toMap
            )
        assert(result.size == 1, s"result size was: ${result.size} when revolving name $name")
        result.head
      case TriggerType =>
        ResolveByTriggerInput(name)
      case CalculationServiceType =>
        ???

  // api
  
  def sequence(namesToResolve: Set[Name], namesSuppliedByTrigger: Set[Name]): Either[Error,Vector[ResolveMethod]] =
    try
      val graph = buildGraph(namesToResolve)
      if graph.isCyclicFor(namesToResolve) then
        Left(Error.UnresolvableCyclicDependency(graph.keySet))
      else
        Right(graph.sequenceFor(namesSuppliedByTrigger).map(resolveMethodFor))
    catch
      case DoesNotExist(name) => Left(Error.UnresolvableUndefinedDependency(name))
