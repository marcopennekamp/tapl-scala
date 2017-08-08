package chapter7

trait Evaluator {
  def evaluate(term: Term): EvaluationResult
}
