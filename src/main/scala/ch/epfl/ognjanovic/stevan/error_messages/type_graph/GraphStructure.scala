package ch.epfl.ognjanovic.stevan.error_messages.type_graph

import ch.epfl.ognjanovic.stevan.error_messages.constraint_language.ConstraintLanguage
import ch.epfl.ognjanovic.stevan.error_messages.inox.{Constraints, SimpleTypes}

trait GraphStructure {
  self: ConstraintLanguage with SimpleTypes with Constraints =>

  import Assertions.Assertion
  import SimpleTypes._
  import Hypotheses._
  import Edges._
  import TypeClasses._
  import Nodes._

  trait Node
  object Nodes {
    case class TypeNode(tpe: SimpleTypes.Type) extends Node
    case class TypeClassNode(typeClass: TypeClass) extends Node
    final case class TypeOptionsNode private(private val identifier: Int) extends Node {
      override def equals(that: Any): Boolean =
        that.isInstanceOf[Unknown] && that.asInstanceOf[TypeOptionsNode].identifier == identifier
      override def hashCode(): Int = identifier
      override def toString: String = "TypeOptionsNode(" + identifier + ")"

      def copy(): TypeOptionsNode = new TypeOptionsNode(identifier)
    }
    object TypeOptionsNode {
      private var next: Int = 0

      def fresh: TypeOptionsNode = synchronized {
        val ret = next
        next += 1
        new TypeOptionsNode(ret)
      }
    }

  }

  trait Edge
  object Edges {
    object ConstructorEdgeDirection {
      trait Direction
      case object Original extends Direction
      case object Decompositional extends Direction
    }
    import ConstructorEdgeDirection._
    case class LessEqualEdge(from: Node, to: Node, hypothesis: Hypothesis) extends Edge
    case class ConstructorEdge(parent: Node, subElement: Node, direction: Direction) extends Edge

  }

  class Graph(val nodes: Set[Node], val edges: Set[Edge]) {
    def union(other: Graph): Graph = {
      Graph(this.nodes union other.nodes, this.edges union other.edges)
    }
  }

  object Graph {
    def apply(nodes: Set[Node], edges: Set[Edge]) = new Graph(nodes, edges)
    def apply(node: Node) = new Graph(Set(node), Set.empty)
    def apply(edge: Edge) = new Graph(Set.empty, Set(edge))
    def empty(): Graph = Graph(Set.empty[Node], Set.empty[Edge])
  }

  def construct(goal: Seq[Assertion]): Graph = goal.map(a => construct(a)).foldLeft(Graph.empty())((a: Graph, b: Graph) => a union b)

  def constructSeq(types: Seq[SimpleTypes.Type]): Seq[(Node, Graph)] = types.map(tpe => construct(tpe))

  def construct(assertion: Assertion): Graph = assertion.constraint match {
    case Constraints.Equals(left, right) =>
      val (leftNode, leftGraph) = construct(left)
      val (rightNode, rightGraph) = construct(right)
      leftGraph union rightGraph union Graph(Set.empty,
        Set(
          LessEqualEdge(leftNode, rightNode, assertion.hypothesis),
          LessEqualEdge(rightNode, leftNode, assertion.hypothesis)
        ))
    case Constraints.HasClass(elem, typeClass) =>
      val (elemNode, elemGraph) = construct(elem)
      val typeClassNode = TypeClassNode(typeClass)
      elemGraph union Graph(typeClassNode) union Graph(LessEqualEdge(elemNode, typeClassNode, assertion.hypothesis))
    case Constraints.OneOf(unknown, tpe, options) =>
      val (unknownNode, unknownGraph) = construct(unknown)
      val (tpeNode, tpeGraph) = construct(tpe)
      val optionsNode = TypeOptionsNode.fresh
      val optionsNodeGraphPairs = constructSeq(options)

      val oneOfGraph = (unknownGraph
        union tpeGraph
        union Graph(LessEqualEdge(unknownNode, tpeNode, assertion.hypothesis))
        union Graph(LessEqualEdge(tpeNode, unknownNode, assertion.hypothesis))
        union Graph(optionsNode)
        union Graph(LessEqualEdge(tpeNode, optionsNode, assertion.hypothesis))
        )

      optionsNodeGraphPairs.foldLeft(oneOfGraph)((graph, pair) => graph
        union pair._2
        union Graph(LessEqualEdge(optionsNode, pair._1, assertion.hypothesis)))
    case _ =>
      // ignore exists currently
      Graph.empty()
  }

  def construct(tpe: SimpleTypes.Type): (Node, Graph) = tpe match {
    case SimpleTypes.FunctionType(froms, to) =>
      val (toNode, toGraph) = construct(to)
      val funNode = TypeNode(tpe)

      val startGraph = (Graph(funNode)
        union toGraph
        union Graph(ConstructorEdge(funNode, toNode, ConstructorEdgeDirection.Original))
        union Graph(ConstructorEdge(funNode, toNode, ConstructorEdgeDirection.Decompositional))
        )

      (funNode,
        froms.foldLeft(startGraph)((graph, elem) => {
          val (elemNode, elemGraph) = construct(elem)
          (graph
            union elemGraph
            union Graph(ConstructorEdge(funNode, elemNode, ConstructorEdgeDirection.Original))
            union Graph(ConstructorEdge(funNode, elemNode, ConstructorEdgeDirection.Decompositional))
            )

        })
      )
    case SimpleTypes.TupleType(elems) =>
      val tupleType = TypeNode(tpe)
      (tupleType,
        elems.foldLeft(Graph(tupleType))((graph, elem) => {
          val (elemNode, elemGraph) = construct(elem)
          (graph
            union elemGraph
            union Graph(ConstructorEdge(tupleType, elemNode, ConstructorEdgeDirection.Original))
            union Graph(ConstructorEdge(tupleType, elemNode, ConstructorEdgeDirection.Decompositional))
            )

        })
      )
    case SimpleTypes.BagType(elemType) =>
      val bagNode = TypeNode(tpe)
      val (elemNode, elemGraph) = construct(elemType)
      (bagNode,
        Graph(bagNode)
          union elemGraph
          union Graph(ConstructorEdge(bagNode, elemNode, ConstructorEdgeDirection.Original))
          union Graph(ConstructorEdge(bagNode, elemNode, ConstructorEdgeDirection.Decompositional))
      )
    case SimpleTypes.SetType(elemType) =>
      val setNode = TypeNode(tpe)
      val (elemNode, elemGraph) = construct(elemType)
      (setNode,
        Graph(setNode)
          union elemGraph
          union Graph(ConstructorEdge(setNode, elemNode, ConstructorEdgeDirection.Original))
          union Graph(ConstructorEdge(setNode, elemNode, ConstructorEdgeDirection.Decompositional))
      )
    case SimpleTypes.ADTType(_, args) =>
      val adtNode = TypeNode(tpe)
      (adtNode,
        args.foldLeft(Graph(adtNode))((graph, elem) => {
          val (elemNode, elemGraph) = construct(elem)
          (graph
            union elemGraph
            union Graph(ConstructorEdge(adtNode, elemNode, ConstructorEdgeDirection.Original))
            union Graph(ConstructorEdge(adtNode, elemNode, ConstructorEdgeDirection.Decompositional))
            )

        })
      )
    case SimpleTypes.MapType(from, to) =>
      val (fromNode, fromGraph) = construct(from)
      val (toNode, toGraph) = construct(to)
      val mapNode = TypeNode(tpe)
      (mapNode,
        Graph(mapNode)
          union fromGraph
          union toGraph
          union Graph(ConstructorEdge(mapNode, fromNode, ConstructorEdgeDirection.Original))
          union Graph(ConstructorEdge(mapNode, fromNode, ConstructorEdgeDirection.Decompositional))
          union Graph(ConstructorEdge(mapNode, toNode, ConstructorEdgeDirection.Original))
          union Graph(ConstructorEdge(mapNode, toNode, ConstructorEdgeDirection.Decompositional))
      )
    case a: SimpleTypes.Type => (TypeNode(a), Graph(Set(TypeNode(a)), Set.empty))
  }


}
