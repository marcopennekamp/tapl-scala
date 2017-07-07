package chapter4

import Term._

object SmallStep {

  /**
    * Evaluate the term via small-step semantics.
    */
  def evaluate(term: Term): EvaluationResult = {
    var termOpt = Option(term)
    var lastTerm = term
    while (termOpt.isDefined) {
      val current = termOpt.get
      lastTerm = current
      termOpt = step(current)
    }

    // At this point, termOpt is a None. So lastTerm is our result term. We need to decide
    // whether the evaluation resulted in a failure or in a value. To do this, we simply
    // check whether our result is a value or not.
    val result = lastTerm
    if (isValue(result)) EvaluationSuccess(result)
    else EvaluationFailure(result)
  }

  /**
    * Execute one step of our small-step semantics. If the term is ill-formed or if no further reductions
    * can be applied, None is returned. Otherwise, exactly one step will be applied.
    */
  private def step(term: Term): Option[Term] = term match {
    // E-IfTrue
    case If(_, True(_), t, _) => Some(t)

    // E-IfFalse
    case If(_, False(_), _, t) => Some(t)

    // E-If
    case If(info, cond, t2, t3) => step(cond).map(cond2 => If(info, cond2, t2, t3))

    // E-Succ
    case Succ(info, t) => step(t).map(t2 => Succ(info, t2))

    // E-PredZero
    case Pred(_, Zero(_)) => Some(Zero(dummyInfo))

    // E-PredSucc
    case Pred(_, Succ(_, n)) if isNumericValue(n) => Some(n)

    // E-Pred
    case Pred(info, t) => step(t).map(t2 => Pred(info, t2))

    // E-IsZeroZero
    case IsZero(_, Zero(_)) => Some(True(dummyInfo))

    // E-IsZeroSucc
    case IsZero(_, Succ(_, n)) if isNumericValue(n) => Some(False(dummyInfo))

    // E-IsZero
    case IsZero(info, t) => step(t).map(t2 => IsZero(info, t2))

    // In all other cases, the term cannot be reduced further.
    case _ => None
  }

}
