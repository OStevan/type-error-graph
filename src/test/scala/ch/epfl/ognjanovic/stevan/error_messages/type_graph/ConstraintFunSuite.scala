package ch.epfl.ognjanovic.stevan.error_messages.type_graph

class ConstraintFunSuite extends GraphFunSuite {

  import Hypotheses._

  test("equality constraint") {
    val first = SimpleTypes.Unknown.fresh
    val second = SimpleTypes.Unknown.fresh
    val res = construct(Constraints.Equals(first, second), Hypothesis(Seq.empty))

    graphCheck(res, 2, 2, 2)
  }

  test("has class constraint") {
    val tpe = SimpleTypes.Unknown.fresh
    val res = construct(Constraint.isNumeric(tpe), Hypothesis(Seq.empty))

    graphCheck(res, 2, 1, 1)
  }

  test("one of constraint") {
    val constraint = Constraints.OneOf(SimpleTypes.Unknown.fresh, SimpleTypes.Unknown.fresh,
      Seq.fill(3)(SimpleTypes.Unknown.fresh))
    val res = construct(constraint, Hypothesis(Seq.empty))

    graphCheck(res,6, 6, 6, 0, 0)
  }
}
