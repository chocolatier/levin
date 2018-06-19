package levin

object IteratedBruteforce {

  def generateSentences(g : Gram) : List[List[NameG]] = {
    g match {
      case AlternativeG(alternatives) => alternatives.flatMap(generateSentences(_)).toList
      case SequenceG (sequence) => List(sequence.flatMap(generateSentences).flatten.toList)
      case OptionalG(x) => generateSentences(x) ++ List()
      case LoopG(x) => generateSentences(x)
      case x@NameG(_) => List(List(x))
    }
  }
}
