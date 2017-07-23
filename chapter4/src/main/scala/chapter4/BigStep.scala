package chapter4

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

    // B-IfTrue, B-IfFalse
    case t@If(_, condition, thenTerm, elseTerm) =>
      bigStep(condition).evaluateOrFailWith(t) {
        case True(_) => bigStep(thenTerm)
        case False(_) => bigStep(elseTerm)
        case _ => EvaluationFailure(t)
      }

    // B-Succ
    case t@Succ(info, t1) =>
      bigStep(t1).evaluateOrFailWith(t) {
        case nv if isNumericValue(nv) => EvaluationSuccess(Succ(info, nv))
        case _ => EvaluationFailure(t)
      }

    // B-PredZero, B-PredSucc
    case t@Pred(_, t1) =>
      bigStep(t1).evaluateOrFailWith(t) {
        case Zero(_) => EvaluationSuccess(Zero(dummyInfo))
        case Succ(_, nv) if isNumericValue(nv) => EvaluationSuccess(nv)
        case _ => EvaluationFailure(t)
      }

    // B-IsZeroZero, B-IsZeroSucc
    case t@IsZero(_, t1) =>
      bigStep(t1).evaluateOrFailWith(t) {
        case Zero(_) => EvaluationSuccess(True(dummyInfo))
        case Succ(_, nv) if isNumericValue(nv) => EvaluationSuccess(False(dummyInfo))
        case _ => EvaluationFailure(t)
      }

    case t => EvaluationFailure(t)
  }

}
