package chapter7

case class Binding()

case class ContextEntry(variableName: String, binding: Binding)

/**
  * A context representing the current variable bindings in order of binding,
  * with the innermost bind being the last entry.
  */
class Context(entries: Seq[ContextEntry]) {

  /**
    * @return The length of the context.
    */
  def length = entries.length

  /**
    * @param index The De Bruijn index of the variable.
    */
  def apply(index: Int): ContextEntry = entries(index)

  /**
    * @return The names of the variables in the context.
    */
  def names = entries.map(_.variableName)

  /**
    * @param name The name x that the new name x' should be similar to.
    * @return A new context with the name x' and x' as a String.
    */
  def pickFreshName(name: String): (Context, String) = {
    var counter = 0
    def createName(c: Int) = if (c > 0) name + c else name
    while (names.contains(createName(counter))) {
      counter += 1
    }

    val namePrime = createName(counter)
    val entry = ContextEntry(namePrime, Binding())
    (new Context(entry +: entries), namePrime)
  }

}
