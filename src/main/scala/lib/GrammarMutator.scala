package levin

object GrammarMutator {
  def mutateGrammar (g: Grammar, start : String = "Expr") = {
    val startGram = g.exprMap(start)

  }

  def chainExprs(g: Grammar, s1 : Gram, s2: Gram) : Grammar  = {
    val chainedExpr = SequenceG (Seq(s1, s2))
    addNewExpr(g, chainedExpr)
  }

  def generateAlternate(g: Grammar, s1: Gram, s2: Gram) : Grammar = {
    val alternatedExpr = AlternativeG (Set(s1, s2))
    addNewExpr(g, alternatedExpr)
  }

  def makeOptional (g : Grammar, s1 : Gram) : Grammar = {
    val optionalExpr = OptionalG (s1)
    addNewExpr(g, optionalExpr)
  }

  def makeLoop (g : Grammar, s1 : Gram) : Grammar = {
    val loopedExpr = LoopG (s1)
    addNewExpr(g, loopedExpr)
  }

  def addNewExpr(g: Grammar, gm : Gram) = {
    val exprName = generateExprName(g)
    Grammar (g.exprMap + (exprName -> gm), g.terminalMap)
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
