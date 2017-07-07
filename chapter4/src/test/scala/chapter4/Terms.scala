package chapter4

import Term._

object Terms {
  val di = dummyInfo
  val ifFalse = If(di, False(di), False(di), False(di))
  val ifTrueIfFalse = If(di, True(di), ifFalse, True(di))
  val ifTrueTrue = If(di, True(di), True(di), ifFalse)
  val predSuccPredZero = Pred(di, Succ(di, Pred(di, Zero(di))))
  val complex = If(di, IsZero(di, predSuccPredZero), Succ(di, Zero(di)), Succ(di, Succ(di, Zero(di))))
  val invalidCond = If(di, Zero(di), False(di), True(di))
  val invalidNum = Succ(di, Pred(di, Succ(di, True(di))))
}
