package levin

import levin.TestCaseGenerator._

import levin.S2EConfig._
import levin.S2EInputGenerator._
import levin.S2EGenerateGrammar._

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

  def concretiseSentence(s : List[NameG], g : Grammar) = {
    val tempSeq = SequenceG(s.toSeq)
    expandExpression(g, tempSeq)
  }

  def runS2E(length : Int, config : S2EConfig, g : Grammar, start : String = "Expr") = {
    val sentences = generateSentences(g.exprMap(start))
    val concreteSentences = sentences.map(concretiseSentence(_, g))

    val restrictEqLists = concreteSentences.map(generateCharacterVector(_))

    for (x <- restrictEqLists) {
      val updatedPlugins = updatePluginsConfig(config.plugins, "AddEqualityRestriction", "eqVector", x)
      val updatedConfig = config.copy(plugins = updatedPlugins)

      // Small brain : Actually handling the null string
      // Ascended brain : Removing it and appending a string to guarantee it will be non empty
      val name = x + x.init.tail.split(",").map(_.toInt).filter(_ != 0).map(_.toChar).mkString
      println("name is " + name)

      init(length, updatedConfig)
      kqueryToSMT2(length, "./cache/" + levinConf.Executable + "/" + name + "/")
    }

  }
}
