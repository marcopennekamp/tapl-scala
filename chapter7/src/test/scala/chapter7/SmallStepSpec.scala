package chapter7

import Term._
import Terms._
import org.scalatest.{FlatSpec, Matchers}

class SmallStepSpec extends FlatSpec with Matchers {
  "SmallStep" should "evaluate id to id" in {
    SmallStep.evaluate(emptyContext, id) should matchPattern {
      case EvaluationSuccess(`id`) =>
    }
  }

  it should "evaluate (id id) to id" in {
    SmallStep.evaluate(emptyContext, applyIdToItself) should matchPattern {
      case EvaluationSuccess(`id`) =>
    }
  }
}
