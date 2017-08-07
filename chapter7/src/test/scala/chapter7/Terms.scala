package chapter7

import Term._

object Terms {
  val di = dummyInfo
  val emptyContext = new Context(Seq())

  val id = Abs(di, "x", Var(di, 0, 1))
  val applyIdToItself = App(di, id, id)
}
