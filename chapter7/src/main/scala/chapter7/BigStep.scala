package chapter7

import Term._

object BigStep {

  // Evaluate the term via big-step semantics.
  def evaluate(term: Term): EvaluationResult = bigStep(term)

  // The use of evaluateOrFailWith ensures that an unprocessed term is always reproduced in
  // its entirety inside EvaluationFailure. Since we are dealing with big-step semantics here,
  // we assume that the rest term in case of failure is always the full input term.
  private def bigStep(term: Term): EvaluationResult = term match {
    // B-Value
    case t if isValue(t) => EvaluationSuccess(t)

    // B-App
    case t@App(_, t1, t2) =>
      bigStep(t1).evaluateOrFailWith(t) {
        case Abs(_, x, t12) => bigStep(t2).evaluateOrFailWith(t) {
          case v2 if isValue(v2) => bigStep(t12.substitute(0, v2))
          case _ => EvaluationFailure(t)
        }
        case _ => EvaluationFailure(t)
      }

    case t => EvaluationFailure(t)
  }

}
