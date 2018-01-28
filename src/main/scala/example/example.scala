import levin._

import smtlib._
import smtlib.trees.Commands._
import smtlib.trees.Terms._

// Example usage of levin. (Actually just quick and dirty testing).
object Example {
  def main (args: Array[String]): Unit = {
    val is = new java.io.FileReader("src/main/scala/example/queries/example_query.smt2")
    val lexer = new smtlib.lexer.Lexer(is)
    val parser = new smtlib.parser.Parser(lexer)

  var cmd = parser.parseCommand
  while (cmd != null){
    cmd match {
            case Assert (term) => {
              println(simplifications.removeExt(term))
              }
            case _ => println("as")
          }
    cmd = parser.parseCommand
    }
  }
}
