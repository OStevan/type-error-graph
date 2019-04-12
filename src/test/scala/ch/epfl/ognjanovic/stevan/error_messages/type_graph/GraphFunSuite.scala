package ch.epfl.ognjanovic.stevan.error_messages.type_graph

import ch.epfl.ognjanovic.stevan.error_messages.constraint_language.ConstraintLanguage
import ch.epfl.ognjanovic.stevan.error_messages.inox.{Constraints, SimpleTypes}
import org.scalatest.FunSuite

abstract class GraphFunSuite extends FunSuite
  with ConstraintLanguage
  with GraphStructure
  with SimpleTypes
  with Constraints {

  import Edges._

  protected def graphCheck(graph: Graph, nodeCount: Int = 0, edgeCount: Int = 0, leqCount: Int = 0,
                           originalCount: Int = 0, decompositionalCount: Int = 0): Unit = {
    // check number of nodes created
    assert(graph.nodes.size == nodeCount)
    // check no other edges were created
    assert(graph.edges.size == edgeCount)
    // check less of equal edges
    assert(graph.edges.foldLeft(0) {
      case (count, _: LessEqualEdge) => count + 1
      case (count, _) => count
    } == leqCount)
    // check original constructor edges
    assert(graph.edges.foldLeft(0) {
      case (count, ConstructorEdge(_, _, ConstructorEdgeDirection.Original)) => count + 1
      case (count, _) => count
    } == originalCount)
    // check decompositional constructor edges
    assert(graph.edges.foldLeft(0) {
      case (count, ConstructorEdge(_, _, ConstructorEdgeDirection.Decompositional)) => count + 1
      case (count, _) => count
    } == decompositionalCount)
  }
}
