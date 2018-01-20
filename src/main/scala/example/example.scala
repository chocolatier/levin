import levin._

// Example usage of levin. (Actually just quick and dirty testing).
object Example {
  def main (args: Array[String]): Unit = {
    val is = new java.io.FileReader("src/main/scala/example/queries/example_query.smt2")
    val lexer = new smtlib.lexer.Lexer(is)
    val parser = new smtlib.parser.Parser(lexer)

    println(parser.parseScript)

  }
}
