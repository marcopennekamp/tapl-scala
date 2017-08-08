package chapter7

import chapter7.Term._

/**
  * A term with variable names instead of De Bruijn indices.
  */
trait NamedTerm {

  /**
    * Compiles the named term to a term in the given context.
    *
    * @return The term compiled from this named term.
    */
  protected def toTerm(context: Context): Term

  /**
    * Compiles the named term to a term in an empty context.
    */
  def toTerm: Term = toTerm(Context.empty)

}

object NamedTerm {

  case class NVar(info: Info, name: String) extends NamedTerm {
    override protected def toTerm(context: Context): Term = {
      val index = context(name).getOrElse(throw new RuntimeException(s"Variable with the name '$name' not found."))
      Var(info, index)
    }
  }

  object NVar {
    def apply(name: String): NVar = NVar(dummyInfo, name)
  }

  case class NAbs(info: Info, variableName: String, t1: NamedTerm) extends NamedTerm {
    override protected def toTerm(context: Context): Term = {
      val context2 = context.withName(variableName)
      Abs(info, variableName, t1.toTerm(context2))
    }
  }

  object NAbs {
    def apply(variableName: String, t1: NamedTerm): NAbs = NAbs(dummyInfo, variableName, t1)
  }

  case class NApp(info: Info, t1: NamedTerm, t2: NamedTerm) extends NamedTerm {
    override protected def toTerm(context: Context): Term = App(info, t1.toTerm(context), t2.toTerm(context))
  }

  object NApp {
    def apply(t1: NamedTerm, t2: NamedTerm): NApp = NApp(dummyInfo, t1, t2)
  }

}
