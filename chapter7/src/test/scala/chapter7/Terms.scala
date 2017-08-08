package chapter7

import NamedTerm._
import Combinators._

object Terms {
  val termId = id.toTerm
  val termIdId = NApp(id, id).toTerm
  val termIdZ = NApp(id, NApp(id, NAbs("z", NApp(id, NVar("z"))))).toTerm
  val termTru = tru.toTerm
}
