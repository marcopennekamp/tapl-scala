package chapter4

import Term._
import Terms._
import org.scalatest.{FlatSpec, Matchers}

class SmallStepSpec extends FlatSpec with Matchers {
  "SmallStep" should "evaluate ifFalse" in {
    SmallStep.evaluate(ifFalse) should matchPattern {
      case EvaluationSuccess(False(_)) =>
    }
  }

  it should "evaluate ifTrueIfFalse" in {
    SmallStep.evaluate(ifTrueIfFalse) should matchPattern {
      case EvaluationSuccess(False(_)) =>
    }
  }

  it should "evaluate ifTrueTrue" in {
    SmallStep.evaluate(ifTrueTrue) should matchPattern {
      case EvaluationSuccess(True(_)) =>
    }
  }

  it should "evaluate predSuccPredZero" in {
    SmallStep.evaluate(predSuccPredZero) should matchPattern {
      case EvaluationSuccess(Zero(_)) =>
    }
  }

  it should "evaluate complex" in {
    SmallStep.evaluate(complex) should matchPattern {
      case EvaluationSuccess(Succ(_, Zero(_))) =>
    }
  }

  it should "fail on invalidCond" in {
    SmallStep.evaluate(invalidCond) should matchPattern {
      case EvaluationFailure(If(_, Zero(_), False(_), True(_))) =>
    }
  }

  it should "fail on invalidNum" in {
    SmallStep.evaluate(invalidNum) should matchPattern {
      case EvaluationFailure(Succ(_, Pred(_, Succ(_, True(_))))) =>
    }
  }
}
