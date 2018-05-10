package levin

import scala.util.Random

object TestCaseGenerator {
  def generateCase(g: Grammar, start : String = "Expr") : String = {
    val startGram = g.exprMap(start)

    expandExpression(g, startGram)
  }

  def expandExpression (g: Grammar, e : Gram) : String = {
    val rnd = new Random
    e match {
      case AlternativeG (alt) => expandExpression (g, alt.toVector(rnd.nextInt(alt.size)))
      case SequenceG (xs) => xs.map(expandExpression(g,_)).mkString
      case OptionalG (option) => {
        if (rnd.nextBoolean) {
          expandExpression(g, option)
        } else {
          ""
        }
      }
      case LoopG (loopExpr) => {
        val length = rnd.nextInt
        var rv = ""

        for (i <- 0 to length){
          rv += expandExpression(g, loopExpr)
        }
        rv
      }
      case NameG (name) => {
        if (g.terminalMap.contains(name)) {
          resolveTerm(g, name)
        } else {
          expandExpression (g, g.exprMap(name))
        }
      }
    }
  }

  def resolveTerm (g: Grammar, n: String) : String = {
    val rnd = new Random
    val range = g.terminalMap(n)
    val subrange = range(rnd.nextInt(range.size))
    val offset = rnd.nextInt(subrange._2 - subrange._1 + 1)

    (subrange._1 + offset).toChar.toString
  }

}
