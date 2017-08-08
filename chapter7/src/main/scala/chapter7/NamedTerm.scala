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
  def toTerm(context: Context): Term

}

object NamedTerm {

  object syntax {
    implicit class AbsSyntax(variable: Symbol) {
      def ->(n1: NamedTerm): NamedTerm = NAbs(variable.name, n1)
    }

    implicit def toVar(v1: Symbol): NamedTerm = NVar(v1.name)

    // The application operator must have a higher precedence than the abstraction operator.
    // This leads to nicer syntax, i.e. x -> x ! x instead of x -> (x ! x).

    implicit class AppVarSyntax(v1: Symbol) {
      def /(n2: NamedTerm): NamedTerm = NApp(v1, n2)
    }

    implicit class AppSyntax(n1: NamedTerm) {
      def /(n2: NamedTerm): NamedTerm = NApp(n1, n2)
    }
  }

  case class NVar(info: Info, name: String) extends NamedTerm {
    override def toTerm(context: Context): Term = {
      val index = context(name).getOrElse(throw new RuntimeException(s"Variable with the name '$name' not found."))
      Var(info, index)
    }
  }

  object NVar {
    def apply(name: String): NVar = NVar(dummyInfo, name)
  }

  case class NAbs(info: Info, variableName: String, t1: NamedTerm) extends NamedTerm {
    override def toTerm(context: Context): Term = {
      val context2 = context.withName(variableName)
      Abs(info, variableName, t1.toTerm(context2))
    }
  }

  object NAbs {
    def apply(variableName: String, t1: NamedTerm): NAbs = NAbs(dummyInfo, variableName, t1)
  }

  case class NApp(info: Info, t1: NamedTerm, t2: NamedTerm) extends NamedTerm {
    override def toTerm(context: Context): Term = App(info, t1.toTerm(context), t2.toTerm(context))
  }

  object NApp {
    def apply(t1: NamedTerm, t2: NamedTerm): NApp = NApp(dummyInfo, t1, t2)
  }

}
