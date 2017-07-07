package chapter4

trait EvaluationResult

/**
  * @param rest The remaining term that couldn't be reduced further.
  */
case class EvaluationFailure(rest: Term) extends EvaluationResult

/**
  * @param result The result value of the evaluation.
  */
case class EvaluationSuccess(result: Term) extends EvaluationResult
