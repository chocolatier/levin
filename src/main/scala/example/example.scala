import levin._

import java.io._

import smtlib._
import smtlib.trees.Commands._
import smtlib.trees.Terms._
import theories.Core._
import trees.Terms._
import theories.ArraysEx._
import theories.Ints.{IntSort, NumeralLit}

import levin.Transformations._
import levin.analysis._
import levin.GrammarInference._

import play.api.libs.json._

import scala.io.Source

// Example usage of levin. (Actually just quick and dirty testing).
object Example {
  def main (args: Array[String]): Unit = {

    val disjPath = "../grammar-constraint-analysis/state_analysis/disjunct.json"
    val disjFile = Source.fromFile(disjPath).getLines.mkString

    val disj = Json.parse(disjFile).as[List[List[Int]]]


    val bvc = buildGrammarVec("../grammar-constraint-analysis/constraints/smt2/")

    val svc = simplifyGrammarVec(bvc.toList, disj)

    // val m = svc
    val m = bvc.map(_._2)

    val arrangements = m.map(mapToGrammar).distinct.mkString("\n    | ")

    var notEBNF = ""

    for ((k,v) <- t){
      notEBNF += v + " = " + listToASCII(k) + "\n"
    }

    notEBNF += "Expr\n    = " + arrangements

    println(notEBNF)

    val x = Json.toJson(bvc)

    val fw = new java.io.FileWriter("constraintMapping.json")
    fw.write(x.toString)
    fw.close

    val fw2 = new java.io.FileWriter("grammar.notebnf")
    fw2.write(notEBNF)
    fw2.close

  }

  def testGetCommonPatterns (path : String) = {
    val files  = new File(path).listFiles.filter(_.getName.endsWith(".smt2"))

    var list = scala.collection.mutable.ListBuffer.empty[Seq[Term]]

    for (f <- files) {
      val is = new java.io.FileReader(f)
      val lexer = new smtlib.lexer.Lexer(is)
      val parser = new smtlib.parser.Parser(lexer)

      var cmd = parser.parseCommand

      while (cmd != null){
        cmd match {
                case Assert (term) => {
                  val v = andToVec2(stripLets(term))
                  list += v
                  }
                case _ =>
            }
        cmd = parser.parseCommand
        }

    }
    println(analysis.patternFrequency(list.toList))

  }

  def testImplications () = {
    // Manual check against for size 4 against166 [State 822] TestCaseGenerator: v0___symfile____tmp_input___0_1_symfile___0 = {0x28, 0x35, 0x20, 0x29}; (int32_t) 689976616, (string) "(5 )"
    // val is = new java.io.FileReader("rebitvec.smt2")
    val is = new java.io.FileReader("../grammar-learning-dump/outs/no-eval-32-static/size-4/constraints-822.smt2")
    val lexer = new smtlib.lexer.Lexer(is)
    val parser = new smtlib.parser.Parser(lexer)

    var cmd = parser.parseCommand

    val fw = new java.io.FileWriter("testImplications.smt2")

    while (cmd != null) {
      cmd match {
        case Assert (term) => {
          println(term.getClass)
          val i = buildImplication(term)
          fw.write (Assert (i).toString)
        }
        case _ => fw.write(cmd.toString)
      }
      cmd = parser.parseCommand
    }

    fw.close
  }

}
