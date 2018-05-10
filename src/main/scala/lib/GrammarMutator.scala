package levin

import levin.TestCaseGenerator._

import sys.process._
import scala.language.postfixOps

import scala.util.Random

object GrammarMutator {
  def mutateGrammar (g: Grammar, start : String = "Expr") = {
    val startGram = g.exprMap(start)
    for (i <- 0 to 100){
      val mutateable = pickGramElement (startGram)

      println("mutating")

      mutateable match {
        case SequenceG(x) => {
          println ("found sequence")
          pprint.pprintln(sequenceMutator (g, SequenceG(x)), height = 9999)

        }
        case _ =>
    }
  }

  }

  def pickGramElement (gm : Gram) : Gram = {
    val rnd = new Random
    gm match {
      case AlternativeG (alt) => alt.toVector(rnd.nextInt(alt.size))
      case x => x
    }
  }

  def sequenceMutator (g: Grammar, seq : SequenceG) : Grammar = {
    println ("mutating sequence")
    checkPotentialLoops (g, seq)
  }

  def checkPotentialLoops (g: Grammar, seq : SequenceG) : Grammar = {
    println("checking for loops")
    var targetGrammar = g
    val repeats = detectRepeats(seq)
    println(repeats)
    for (repeat <- repeats) {
      val mid = Seq(LoopG (repeat))
      val iS = seq.sequence.indexOfSlice(Seq(repeat, repeat))
      val left = seq.sequence.take(iS)
      val right = seq.sequence.drop(iS).dropWhile(_==repeat)
      val updatedGram = SequenceG (left ++ mid ++ right)

      println (updatedGram)

      val name = generateExprName(g)
      val next_grammar = Grammar (g.exprMap + (name -> updatedGram), g.terminalMap)
      if (testGrammar(next_grammar, name)) {
        targetGrammar = next_grammar
      }
    }

    targetGrammar
  }

  def testGrammar (g: Grammar, start: String = "Expr") : Boolean = {
    val targetProgram = (" ")

    for (i <- 0 to 20) {
      val testCase = generateCase(g, start)
      val testCaseFW = new java.io.FileWriter("testFile")
      println (testCase)
      testCaseFW.write(testCase)
      testCaseFW.close
      val s = targetProgram + " testFile" !;
      println(s)

      if (s != 0) {
        return false
      }
    }
    return true
  }

  def detectRepeats (seq : SequenceG) : Seq [Gram] = {
    if (seq.sequence.length <= 1) {
      Seq ()
    } else {
      seq.sequence.sliding(2).filter {case Seq(a, b) => a == b}.map {case Seq(a, b) => a} .toSeq
    }
  }

  def makeSequence (g: Grammar, s1 : Gram, s2: Gram) : Grammar  = {
    val chainedExpr = SequenceG (Seq(s1, s2))
    addNewExpr(g, chainedExpr)
  }

  def makeAlternative (g: Grammar, s1: Gram, s2: Gram) : Grammar = {
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

    if (taken.isEmpty || taken.last.filter(_.isDigit) == "") {
      "e0"
    } else {
      val n = taken.last.filter(_.isDigit).toInt
      "e" + (n + 1).toString
    }
  }
}
