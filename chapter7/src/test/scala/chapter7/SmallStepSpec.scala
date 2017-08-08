package chapter7

import Term._
import Terms._
import org.scalatest.{FlatSpec, Matchers}

class SmallStepSpec extends FlatSpec with Matchers {
  "SmallStep" should "evaluate id to id" in {
    SmallStep.evaluate(Context.empty, termId) should matchPattern {
      case EvaluationSuccess(`termId`) =>
    }
  }

  it should "evaluate (id id) to id" in {
    SmallStep.evaluate(Context.empty, termIdId) should matchPattern {
      case EvaluationSuccess(`termId`) =>
    }
  }

  it should "evaluate (id (id (\\z. id z))) to \\z. id z" in {
    SmallStep.evaluate(Context.empty, termIdZ) should matchPattern {
      case EvaluationSuccess(Abs(_, "z", App(_, `termId`, Var(_, 0)))) =>
    }
  }

  it should "evaluate tru to tru" in {
    SmallStep.evaluate(Context.empty, termTru) should matchPattern {
      case EvaluationSuccess(`termTru`) =>
    }
  }
}
