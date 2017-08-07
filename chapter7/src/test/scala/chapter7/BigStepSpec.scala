package chapter7

import Term._
import Terms._
import org.scalatest.{FlatSpec, Matchers}

class BigStepSpec extends FlatSpec with Matchers {
  /* "BigStep" should "evaluate ifFalse" in {
    BigStep.evaluate(ifFalse) should matchPattern {
      case EvaluationSuccess(False(_)) =>
    }
  }

  it should "evaluate ifTrueIfFalse" in {
    BigStep.evaluate(ifTrueIfFalse) should matchPattern {
      case EvaluationSuccess(False(_)) =>
    }
  }

  it should "evaluate ifTrueTrue" in {
    BigStep.evaluate(ifTrueTrue) should matchPattern {
      case EvaluationSuccess(True(_)) =>
    }
  }

  it should "evaluate predSuccPredZero" in {
    BigStep.evaluate(predSuccPredZero) should matchPattern {
      case EvaluationSuccess(Zero(_)) =>
    }
  }

  it should "evaluate complex" in {
    BigStep.evaluate(complex) should matchPattern {
      case EvaluationSuccess(Succ(_, Zero(_))) =>
    }
  }

  it should "fail on invalidCond" in {
    BigStep.evaluate(invalidCond) should matchPattern {
      case EvaluationFailure(If(_, Zero(_), False(_), True(_))) =>
    }
  }

  it should "fail on invalidNum" in {
    BigStep.evaluate(invalidNum) should matchPattern {
      case EvaluationFailure(Succ(_, Pred(_, Succ(_, True(_))))) =>
    }
  } */
}
