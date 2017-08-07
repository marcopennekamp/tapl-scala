package chapter7

trait Term {

  /**
    * @return The term in lambda string form, with variable names.
    */
  def toString(context: Context): String

  /**
    * @return A term shifted by d levels.
    */
  def shift(d: Int): Term = shift(0, d)
  protected def shift(c: Int, d: Int): Term

  /**
    * Realizes a substitution [index -> s]this.
    * @param j The De Bruijn index of the substituted variable.
    * @param s The substituted term.
    */
  def substitute(j: Int, s: Term): Term = substitute(j, s, 0)
  protected def substitute(j: Int, s: Term, d: Int): Term
}

object Term {

  /**
    * @param line The line that the term appears on in the source file.
    * @param column The column of the first character of the term in the source file.
    */
  case class Info(line: Int, column: Int)

  val dummyInfo: Info = Info(0, 0)

  /**
    * A variable term.
    *
    * @param index The De Bruijn index of the variable.
    * @param contextLength The total length of the context in which the variable occurs.
    */
  case class Var(info: Info, index: Int, contextLength: Int) extends Term {
    override def toString(context: Context): String = {
      if (context.length == contextLength) {
        context(index).variableName
      } else {
        "[bad index]"
      }
    }

    override protected def shift(c: Int, d: Int): Term = {
      if (index >= c) {
        Var(info, index + d, contextLength + d)
      } else {
        Var(info, index, contextLength + d)
      }
    }

    override protected def substitute(j: Int, s: Term, d: Int): Term = {
      // Only substitute the term s if the variable index is exactly j + d, i.e. the index j shifted by d.
      // Change nothing if there is no need to shift.
      if (index == j + d) {
        s.shift(d)
      } else {
        this
      }
    }
  }

  /**
    * A lambda abstraction term.
    *
    * @param variableName The parsed name of the variable, used for printing a term.
    */
  case class Abs(info: Info, variableName: String, t1: Term) extends Term {
    override def toString(context: Context): String = {
      val (context2, name2) = context.pickFreshName(variableName)
      "(\u03BB " + name2 + ". " + t1.toString(context2) + ")"
    }

    override protected def shift(c: Int, d: Int): Term = Abs(info, variableName, t1.shift(c + 1, d))
    override protected def substitute(j: Int, s: Term, d: Int): Term = Abs(info, variableName, t1.substitute(j, s, d + 1))
  }

  /**
    * A lambda application term.
    */
  case class App(info: Info, t1: Term, t2: Term) extends Term {
    override def toString(context: Context): String = {
      "(" + t1.toString(context) + " " + t2.toString(context) + ")"
    }

    override protected def shift(c: Int, d: Int): Term = App(info, t1.shift(c, d), t2.shift(c, d))
    override protected def substitute(j: Int, s: Term, d: Int): Term = App(info, t1.substitute(j, s, d), t2.substitute(j, s, d))
  }

  /**
    * @return Whether the given term is a value, i.e. an abstraction.
    */
  def isValue(term: Term): Boolean = term.isInstanceOf[Abs]

}
