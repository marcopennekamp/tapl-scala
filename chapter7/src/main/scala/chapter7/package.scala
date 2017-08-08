package object chapter7 {
  /**
    * @param line The line that the term appears on in the source file.
    * @param column The column of the first character of the term in the source file.
    */
  case class Info(line: Int, column: Int)

  val dummyInfo: Info = Info(0, 0)

  implicit def toTerm(namedTerm: NamedTerm): Term = namedTerm.toTerm(Context.empty)
}
