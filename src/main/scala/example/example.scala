import levin._

import smtlib._
import smtlib.trees.Commands._
import smtlib.trees.Terms._
import java.io._

import levin.Transformations._

// Example usage of levin. (Actually just quick and dirty testing).
object Example {
  def main (args: Array[String]): Unit = {
    val is = new java.io.FileReader("src/main/scala/example/queries/example_query2.smt2")
    val lexer = new smtlib.lexer.Lexer(is)
    val parser = new smtlib.parser.Parser(lexer)

    var cmd = parser.parseCommand
    val fw = new FileWriter("test.txt", true) 

    while (cmd != null){
      cmd match {
              case Assert (term) => {
                // println (Transformations.stripLets(term))
                println (analysis.classify(andToVec2(stripLets(term))))
                }
              case _ => fw.write(cmd.toString)
            }
      cmd = parser.parseCommand
      }
    fw.close()
  }

  def testGetCommonPatterns (path : String) = {
    val files  = new File(path).list.filter(_.endsWith(".smt2"))
    println (files)
  }
}
