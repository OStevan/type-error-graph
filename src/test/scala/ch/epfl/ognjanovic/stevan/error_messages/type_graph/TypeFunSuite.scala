package ch.epfl.ognjanovic.stevan.error_messages.type_graph

import ch.epfl.ognjanovic.stevan.error_messages.inox.FreshIdentifier

class TypeFunSuite extends GraphFunSuite {

  test("simple type") {
    val tpe = SimpleTypes.BooleanType()
    val graph = construct(tpe)

    assert(graph._2.nodes.size == 1)
    assert(graph._2.edges.isEmpty)
  }

  test("function type") {
    val numberOfParameters = 5
    val functionType = SimpleTypes.FunctionType(Seq.fill(numberOfParameters)(SimpleTypes.Unknown.fresh),
      SimpleTypes.Unknown.fresh)
    val res = construct(functionType)

    graphCheck(res._2, 7, 12, 0, 6, 6)
  }

  test("map type") {
    val mapType = SimpleTypes.MapType(SimpleTypes.Unknown.fresh, SimpleTypes.Unknown.fresh)
    val res = construct(mapType)

    graphCheck(res._2, nodeCount = 3, edgeCount = 4, originalCount = 2, decompositionalCount = 2)
  }

  test("bag type") {
    val bagType = SimpleTypes.BagType(SimpleTypes.Unknown.fresh)
    val res = construct(bagType)

    graphCheck(res._2, nodeCount = 2, edgeCount = 2, originalCount = 1, decompositionalCount = 1)
  }

  test("set type") {
    val setType = SimpleTypes.SetType(SimpleTypes.Unknown.fresh)
    val res = construct(setType)

    graphCheck(res._2, nodeCount = 2, edgeCount = 2, originalCount = 1, decompositionalCount = 1)
  }

  test("adt type") {
    val adtType = SimpleTypes.ADTType(FreshIdentifier("test"), Seq.fill(2)(SimpleTypes.Unknown.fresh))
    val res = construct(adtType)

    graphCheck(res._2, nodeCount = 3, edgeCount = 4, originalCount = 2, decompositionalCount = 2)
  }

  test("tuple type") {
    val tupleType = SimpleTypes.TupleType(Seq.fill(3)(SimpleTypes.Unknown.fresh))
    val res = construct(tupleType)

    graphCheck(res._2, 4, 6, 0, 3, 3)
  }
}
