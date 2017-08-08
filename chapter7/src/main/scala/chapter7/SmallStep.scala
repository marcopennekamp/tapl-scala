package chapter7

import Term._

object SmallStep extends Evaluator {

  /**
    * Evaluate the term via small-step semantics.
    */
  override def evaluate(term: Term): EvaluationResult = {
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
    // E-AppAbs
    case App(_, Abs(_, _, t12), v2) if isValue(v2) =>
      // First we need to shift the term by 1, because it will be inserted one lambda abstraction down.
      val shifted = v2.shift(1)

      // Then we need to substitute the variable x (represented by the index 0) by the value v2.
      val subsTerm = t12.substitute(0, shifted)

      // Finally, we have to shift back by one, because the lambda abstraction was "lost" in the application.
      Some(subsTerm.shift(-1))

    // E-App2
    case App(info, v1, t2) if isValue(v1) => step(t2).map(t => App(info, v1, t))

    // E-App1
    case App(info, t1, t2) => step(t1).map(t => App(info, t, t2))

    case _ => None
  }

}
