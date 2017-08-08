package chapter7

import Combinators._

object Terms {
  import NamedTerm.syntax._
  val id: Term = cid
  val idId: Term = cid / cid
  val idZ: Term = cid / (cid / ('z -> cid / 'z))
  val tru: Term = ctru
  val fls: Term = cfls
  val testTru: Term = ctest / ctru / ctru / cfls
  val testFls: Term = ctest / cfls / ctru / cfls
  val andTruTru: Term = cand / ctru / ctru
  val andTruFls: Term = cand / ctru / cfls
  val andFlsTru: Term = cand / cfls / ctru
  val andFlsFls: Term = cand / cfls / cfls
  val orTruTru: Term = cor / ctru / ctru
  val orTruFls: Term = cor / ctru / cfls
  val orFlsTru: Term = cor / cfls / ctru
  val orFlsFls: Term = cor / cfls / cfls
  val notTru: Term = cnot / ctru
  val notFls: Term = cnot / cfls
}
