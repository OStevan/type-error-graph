package ch.epfl.ognjanovic.stevan.error_messages.type_graph

class GoalFunSuite extends GraphFunSuite {

  import Assertions._
  import Hypotheses._
  import Edges._

  test("Equality graph") {
    val tpe = SimpleTypes.StringType()
    val unknown = SimpleTypes.Unknown.fresh
    val equality = Constraints.Equals(tpe, unknown)

    val graph = construct(Seq(Assertion(Hypothesis(Seq.empty), equality)))
    assert(graph.nodes.size == 2 && graph.edges.size == 2)
  }

  test("Test union of two constraints") {
    val first = SimpleTypes.Unknown.fresh
    val second = SimpleTypes.Unknown.fresh
    val tpe = SimpleTypes.StringType()
    val firstEquality = Constraints.Equals(first, tpe)
    val secondEquality = Constraints.Equals(second, tpe)

    val graph = construct(Seq(Assertion(Hypothesis(Seq.empty), firstEquality), Assertion(Hypothesis(Seq.empty), secondEquality)))
    assert(graph.nodes.size == 3 && graph.edges.size == 4)
  }

  test("test constructor edge creation") {
    val mapType = SimpleTypes.MapType(SimpleTypes.IntegerType(), SimpleTypes.StringType())
    val unknown = SimpleTypes.Unknown.fresh
    val equality = Constraints.Equals(unknown, mapType)
    val graph = construct(Seq(Assertion(Hypothesis(Seq.empty), equality)))


    // check number of created nodes, one for unknown, integer, string and map
    assert(graph.nodes.size == 4)

    // check number of original constructor edges
    assert(graph.edges.foldRight(0) {
      case (_: LessEqualEdge, count) => count
      case (ConstructorEdge(_, _, ConstructorEdgeDirection.Original), count) => count + 1
      case (ConstructorEdge(_, _, ConstructorEdgeDirection.Decompositional), count) => count
    } == 2)

    // check number of decompositional constructor edges
    assert(graph.edges.foldRight(0) {
      case (_: LessEqualEdge, count) => count
      case (ConstructorEdge(_, _, ConstructorEdgeDirection.Original), count) => count
      case (ConstructorEdge(_, _, ConstructorEdgeDirection.Decompositional), count) => count + 1
    } == 2)

    // check total number of edges
    assert(graph.edges.size == 6)
  }

  test("no node replication") {
    val int1Type = SimpleTypes.IntegerType()
    val int2Type = SimpleTypes.IntegerType()
    val stringType = SimpleTypes.StringType()
    val unknown = SimpleTypes.Unknown.fresh

    val graph = construct(Seq(
      Assertion(Hypothesis(Seq.empty), Constraints.Equals(int1Type, unknown)),
      Assertion(Hypothesis(Seq.empty), Constraints.Equals(stringType, unknown)),
      Assertion(Hypothesis(Seq.empty), Constraints.Equals(int1Type, int2Type))
    ))
    assert(graph.nodes.size == 3)
  }

}
