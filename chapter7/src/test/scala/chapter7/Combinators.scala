package chapter7

import NamedTerm.syntax._

object Combinators {
  val cid = 'x -> 'x
  val ctru = 't -> ('f -> 't)
  val cfls = 't -> ('f -> 'f)
  val ctest = 'l -> ('m -> ('n -> 'l / 'm / 'n))
  val cand = 'b -> ('c -> 'b / 'c / cfls)
  val cor = 'b -> ('c -> 'b / ctru / 'c)
  val cnot = 'b -> 'b / cfls / ctru

  def print(): Unit = {
    val cs = Seq(("id", cid), ("tru", ctru), ("fls", cfls), ("test", ctest), ("and", cand), ("or", cor), ("not", cnot))
    cs.foreach { case (name, c) => println(s"$name = ${c: Term}") }
  }
}
