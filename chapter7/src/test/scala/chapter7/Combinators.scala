package chapter7

import chapter7.NamedTerm.{NAbs, NVar}

object Combinators {
  val id = NAbs("x", NVar("x"))
  val tru = NAbs("t", NAbs("f", NVar("t")))
}
