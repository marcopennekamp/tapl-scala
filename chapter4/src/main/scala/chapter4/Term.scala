package chapter4

trait Term

object Term {
  /**
    * @param line The line that the term appears on in the source file.
    * @param column The column of the first character of the term in the source file.
    */
  case class Info(line: Int, column: Int)

  val dummyInfo: Info = Info(0, 0)

  case class True(info: Info) extends Term
  case class False(info: Info) extends Term
  case class If(info: Info, condition: Term, thenTerm: Term, elseTerm: Term) extends Term
  case class Zero(info: Info) extends Term
  case class Succ(info: Info, term: Term) extends Term
  case class Pred(info: Info, term: Term) extends Term
  case class IsZero(info: Info, term: Term) extends Term

  def isValue(term: Term): Boolean = isNumericValue(term) || isBooleanValue(term)

  def isBooleanValue(term: Term): Boolean = term match {
    case True(_) => true
    case False(_) => true
    case _ => false
  }

  def isNumericValue(term: Term): Boolean = term match {
    case Zero(_) => true
    case Succ(_, t) => isNumericValue(t)
    case _ => false
  }
}
