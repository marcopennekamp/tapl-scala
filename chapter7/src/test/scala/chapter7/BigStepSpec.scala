package chapter7

import Term._
import Terms._
import org.scalatest.{FlatSpec, Matchers}

class BigStepSpec extends FlatSpec with Matchers {
  "BigStep" should "evaluate id to id" in {
    BigStep.evaluate(id) should matchPattern {
      case EvaluationSuccess(`id`) =>
    }
  }

  it should "evaluate (id id) to id" in {
    BigStep.evaluate(idId) should matchPattern {
      case EvaluationSuccess(`id`) =>
    }
  }

  it should "evaluate (id (id (\\z. id z))) to \\z. id z" in {
    BigStep.evaluate(idZ) should matchPattern {
      case EvaluationSuccess(Abs(_, "z", App(_, `id`, Var(_, 0)))) =>
    }
  }

  it should "evaluate tru to tru" in {
    BigStep.evaluate(tru) should matchPattern {
      case EvaluationSuccess(`tru`) =>
    }
  }

  it should "evaluate testTru to tru" in {
    BigStep.evaluate(testTru) should matchPattern {
      case EvaluationSuccess(`tru`) =>
    }
  }

  it should "evaluate testFls to fls" in {
    BigStep.evaluate(testFls) should matchPattern {
      case EvaluationSuccess(`fls`) =>
    }
  }

  it should "evaluate andTruTru to tru" in {
    BigStep.evaluate(andTruTru) should matchPattern {
      case EvaluationSuccess(`tru`) =>
    }
  }

  it should "evaluate andTruFls to fls" in {
    BigStep.evaluate(andTruFls) should matchPattern {
      case EvaluationSuccess(`fls`) =>
    }
  }

  it should "evaluate andFlsTru to fls" in {
    BigStep.evaluate(andFlsTru) should matchPattern {
      case EvaluationSuccess(`fls`) =>
    }
  }

  it should "evaluate andFlsFls to fls" in {
    BigStep.evaluate(andFlsFls) should matchPattern {
      case EvaluationSuccess(`fls`) =>
    }
  }

  it should "evaluate orTruTru to tru" in {
    BigStep.evaluate(orTruTru) should matchPattern {
      case EvaluationSuccess(`tru`) =>
    }
  }

  it should "evaluate orTruFls to tru" in {
    BigStep.evaluate(orTruFls) should matchPattern {
      case EvaluationSuccess(`tru`) =>
    }
  }

  it should "evaluate orFlsTru to tru" in {
    BigStep.evaluate(orFlsTru) should matchPattern {
      case EvaluationSuccess(`tru`) =>
    }
  }

  it should "evaluate orFlsFls to fls" in {
    BigStep.evaluate(orFlsFls) should matchPattern {
      case EvaluationSuccess(`fls`) =>
    }
  }

  it should "evaluate notTru to fls" in {
    BigStep.evaluate(notTru) should matchPattern {
      case EvaluationSuccess(`fls`) =>
    }
  }

  it should "evaluate notFls to tru" in {
    BigStep.evaluate(notFls) should matchPattern {
      case EvaluationSuccess(`tru`) =>
    }
  }
}
