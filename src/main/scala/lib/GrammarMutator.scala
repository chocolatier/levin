package levin

object GrammarMutator {
  def mutateGrammar (g: Grammar, start : String = "Expr") = {
    val startGram = g.exprMap(start)

  }

  def chainExprs(g: Grammar, s1 : Gram, s2: Gram) : Grammar  = {
    val chainedExpr = SequenceG (Seq(s1, s2))
    val exprName = generateExprName(g)
    Grammar (g.exprMap + (exprName -> chainedExpr), g.terminalMap)
  }

  def generateExprName (g: Grammar) : String = {
    val taken = g.exprMap.keys.toList.sorted

    if (taken.isEmpty) {
      "e0"
    } else {
      val n = taken.last.filter(_.isDigit).toInt
      "e" + (n + 1).toString
    }
  }
}
