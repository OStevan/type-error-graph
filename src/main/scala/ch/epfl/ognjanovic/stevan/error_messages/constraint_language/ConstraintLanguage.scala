package ch.epfl.ognjanovic.stevan.error_messages.constraint_language

import ch.epfl.ognjanovic.stevan.error_messages.inox.{Constraints, SimpleTypes}

trait ConstraintLanguage { self: Constraints with SimpleTypes =>

  import SimpleTypes._
  import ConstraintConjuctions._
  import Hypotheses._

  object ConstraintConjuctions {
    case class ConstraintConjuction(constaints: Seq[Constraint])
  }

  sealed trait Axiom
  object Axioms {
    case class Simple(constraint: Constraint) extends Axiom
    case class QuantifiedAxiom(quantifiedVariables: Seq[Unknown], constraintConjuction: ConstraintConjuction,
                               constraint: Constraint) extends Axiom
  }

  object Hypotheses {
    case class Hypothesis(axioms: Seq[Axiom])
  }

  object Assertions {
    case class Assertion(hypothesis: Hypothesis, constraint: Constraint)
  }

}
