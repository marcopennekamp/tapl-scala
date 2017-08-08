package chapter7

trait Term {

  override def toString: String = toString(Context.empty)

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
    * A variable term.
    *
    * @param index The De Bruijn index of the variable.
    */
  case class Var(info: Info, index: Int) extends Term {
    override def toString(context: Context): String = context(index).variableName

    override protected def shift(c: Int, d: Int): Term = {
      if (index >= c) {
        Var(info, index + d)
      } else {
        Var(info, index)
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

  object Var {
    def apply(index: Int): Var = Var(dummyInfo, index)
  }

  /**
    * A lambda abstraction term.
    *
    * @param variableName The parsed name of the variable, used for printing a term.
    */
  case class Abs(info: Info, variableName: String, t1: Term) extends Term {
    override def toString(context: Context): String = {
      val (context2, name2) = context.pickFreshName(variableName)
      "(\u03BB" + name2 + ". " + t1.toString(context2) + ")"
    }

    override protected def shift(c: Int, d: Int): Term = Abs(info, variableName, t1.shift(c + 1, d))
    override protected def substitute(j: Int, s: Term, d: Int): Term = Abs(info, variableName, t1.substitute(j, s, d + 1))
  }

  object Abs {
    def apply(variableName: String, t1: Term): Abs = Abs(dummyInfo, variableName, t1)
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

  object App {
    def apply(t1: Term, t2: Term): App = App(dummyInfo, t1, t2)
  }

  /**
    * @return Whether the given term is a value, i.e. an abstraction.
    */
  def isValue(term: Term): Boolean = term.isInstanceOf[Abs]

}
