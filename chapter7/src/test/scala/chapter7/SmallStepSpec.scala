package chapter7

import Term._
import Terms._
import org.scalatest.{BeforeAndAfterAll, FlatSpec, Matchers}

class SmallStepSpec extends FlatSpec with Matchers with BeforeAndAfterAll {
  override protected def beforeAll(): Unit = {
    Combinators.print()
  }

  "SmallStep" should "evaluate id to id" in {
    SmallStep.evaluate(id) should matchPattern {
      case EvaluationSuccess(`id`) =>
    }
  }

  it should "evaluate (id id) to id" in {
    SmallStep.evaluate(idId) should matchPattern {
      case EvaluationSuccess(`id`) =>
    }
  }

  it should "evaluate (id (id (\\z. id z))) to \\z. id z" in {
    SmallStep.evaluate(idZ) should matchPattern {
      case EvaluationSuccess(Abs(_, "z", App(_, `id`, Var(_, 0)))) =>
    }
  }

  it should "evaluate tru to tru" in {
    SmallStep.evaluate(tru) should matchPattern {
      case EvaluationSuccess(`tru`) =>
    }
  }

  it should "evaluate testTru to tru" in {
    SmallStep.evaluate(testTru) should matchPattern {
      case EvaluationSuccess(`tru`) =>
    }
  }

  it should "evaluate testFls to fls" in {
    SmallStep.evaluate(testFls) should matchPattern {
      case EvaluationSuccess(`fls`) =>
    }
  }

  it should "evaluate andTruTru to tru" in {
    SmallStep.evaluate(andTruTru) should matchPattern {
      case EvaluationSuccess(`tru`) =>
    }
  }

  it should "evaluate andTruFls to fls" in {
    SmallStep.evaluate(andTruFls) should matchPattern {
      case EvaluationSuccess(`fls`) =>
    }
  }

  it should "evaluate andFlsTru to fls" in {
    SmallStep.evaluate(andFlsTru) should matchPattern {
      case EvaluationSuccess(`fls`) =>
    }
  }

  it should "evaluate andFlsFls to fls" in {
    SmallStep.evaluate(andFlsFls) should matchPattern {
      case EvaluationSuccess(`fls`) =>
    }
  }

  it should "evaluate orTruTru to tru" in {
    SmallStep.evaluate(orTruTru) should matchPattern {
      case EvaluationSuccess(`tru`) =>
    }
  }

  it should "evaluate orTruFls to tru" in {
    SmallStep.evaluate(orTruFls) should matchPattern {
      case EvaluationSuccess(`tru`) =>
    }
  }

  it should "evaluate orFlsTru to tru" in {
    SmallStep.evaluate(orFlsTru) should matchPattern {
      case EvaluationSuccess(`tru`) =>
    }
  }

  it should "evaluate orFlsFls to fls" in {
    SmallStep.evaluate(orFlsFls) should matchPattern {
      case EvaluationSuccess(`fls`) =>
    }
  }

  it should "evaluate notTru to fls" in {
    SmallStep.evaluate(notTru) should matchPattern {
      case EvaluationSuccess(`fls`) =>
    }
  }

  it should "evaluate notFls to tru" in {
    SmallStep.evaluate(notFls) should matchPattern {
      case EvaluationSuccess(`tru`) =>
    }
  }
}
