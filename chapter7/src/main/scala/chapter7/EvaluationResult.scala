package chapter7

/**
  * Represents a failed or successful evaluation.
  */
trait EvaluationResult {
  def flatMap(f: Term => EvaluationResult): EvaluationResult
  def map(f: Term => Term): EvaluationResult = flatMap(t => EvaluationSuccess(f(t)))

  def evaluateOrFailWith(failResult: Term)(f: Term => EvaluationResult): EvaluationResult = {
    flatMap(f) match {
      case s@EvaluationSuccess(_) => s
      case EvaluationFailure(_) => EvaluationFailure(failResult)
    }
  }
}

/**
  * @param rest The remaining term that couldn't be reduced further.
  */
case class EvaluationFailure(rest: Term) extends EvaluationResult {
  override def flatMap(f: Term => EvaluationResult): EvaluationResult = this
}

/**
  * @param result The result value of the evaluation.
  */
case class EvaluationSuccess(result: Term) extends EvaluationResult {
  override def flatMap(f: Term => EvaluationResult): EvaluationResult = f(result)
}
