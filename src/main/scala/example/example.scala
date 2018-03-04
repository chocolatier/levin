import levin._

import smtlib._
import smtlib.trees.Commands._
import smtlib.trees.Terms._
import java.io._

import levin.Transformations._

// Example usage of levin. (Actually just quick and dirty testing).
object Example {
  def main (args: Array[String]): Unit = {
    testGetCommonPatterns("../grammar-learning-dump/outs/no-eval-32-static/size-4")
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
}
