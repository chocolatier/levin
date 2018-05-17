package levin

import levin.TestCaseGenerator._
import levin.GrammarMutator._

import sys.process._
import scala.language.postfixOps

import scala.util.Random

object S2EInputGenerator {

  def generateInput (gm : Gram, g : Grammar) : String = {
    val exprName = generateExprName(g)
    val nG = Grammar (g.exprMap + (exprName -> gm), g.terminalMap)
    val newCase = generateCase(nG, exprName)

    println ("Generated:" + newCase)

    newCase
  }

  def generateInputsForGrammar (g : Grammar, start : String) : Seq[String] = {
    val startGram = g.exprMap (start)

    startGram match {
      case AlternativeG(xs) => xs.map (generateInput(_, g)).toSeq
      case _ => Seq(generateInput (startGram, g))
    }

  }
}