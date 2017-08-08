package chapter7

import chapter7.Combinators._
import NamedTerm.syntax._
import org.scalatest.{FlatSpec, Matchers}

abstract class EvaluationSpec(name: String, evaluator: Evaluator) extends FlatSpec with Matchers {
  implicit class EvaluationTest(t: NamedTerm) {
    def check(f: PartialFunction[Term, Boolean]) = {
      evaluator.evaluate(t) should matchPattern {
        case EvaluationSuccess(s) if f.applyOrElse(s, (_: Term) => false) =>
      }
    }

    /**
      * Evaluates t and checks whether it's accepted by f.
      */
    def -->?(f: PartialFunction[Term, Boolean]) = check(f)

    /**
      * Evaluates t and checks whether it equals v.
      */
    def -->(v: Term) = check { case `v` => true }
  }

  name should "evaluate id to id" in { cid --> cid }

  it should "evaluate (id id) to id" in { cid / cid --> cid }

  val idZ = cid / (cid / ('z -> cid / 'z))
  val idZResult = 'z -> cid / 'z
  it should s"evaluate $idZ to $idZResult" in { idZ --> idZResult }

  it should "evaluate tru to tru" in { ctru --> ctru }
  it should "evaluate fls to fls" in { cfls --> cfls }

  it should "evaluate 'test' correctly " in {
    ctest / ctru / ctru / cfls --> ctru
    ctest / cfls / ctru / cfls --> cfls
  }
  it should "evaluate 'and' correctly" in {
    cand / ctru / ctru --> ctru
    cand / ctru / cfls --> cfls
    cand / cfls / ctru --> cfls
    cand / cfls / cfls --> cfls
  }

  it should "evaluate 'or' correctly" in {
    cor / ctru / ctru --> ctru
    cor / ctru / cfls --> ctru
    cor / cfls / ctru --> ctru
    cor / cfls / cfls --> cfls
  }

  it should "evaluate 'not' correctly" in {
    cnot / ctru --> cfls
    cnot / cfls --> ctru
  }
}
