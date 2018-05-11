// import levin._
//
// import java.io._
//
// import smtlib._
// import smtlib.trees.Commands._
// import smtlib.trees.Terms._
// import theories.Core._
// import trees.Terms._
// import theories.ArraysEx._
// import theories.Ints.{IntSort, NumeralLit}
//
// import levin.Transformations._
// import levin.analysis._
// import levin.GrammarInference._
// import levin.TestCaseGenerator._
// import levin.GrammarMutator._
//
// import play.api.libs.json._
//
// import scala.io.Source
//
// import pprint._
//
// // Example usage of levin. (Actually just quick and dirty testing).
// object Example {
//   def main (args: Array[String]): Unit = {
//
//     val ig = generateInitialGrammar("../grammar-constraint-analysis/constraints/smt2/")
//     val cT = ig.exprMap.values.map(classifyTerms)
//
//     pprint.pprintln (ig, width = 50, height = 99999)
//     pprint.pprintln (cT)
//
//   //   for {
//   //         i <- 0 to 20
//   //       } {
//   //       println(generateCase(ig))
//   // }
//
//   mutateGrammar(ig)
//
//   }
//
//   def testGetCommonPatterns (path : String) = {
//     val files  = new File(path).listFiles.filter(_.getName.endsWith(".smt2"))
//
//     var list = scala.collection.mutable.ListBuffer.empty[Seq[Term]]
//
//     for (f <- files) {
//       val is = new java.io.FileReader(f)
//       val lexer = new smtlib.lexer.Lexer(is)
//       val parser = new smtlib.parser.Parser(lexer)
//
//       var cmd = parser.parseCommand
//
//       while (cmd != null){
//         cmd match {
//                 case Assert (term) => {
//                   val v = andToVec2(stripLets(term))
//                   list += v
//                   }
//                 case _ =>
//             }
//         cmd = parser.parseCommand
//         }
//
//     }
//     println(analysis.patternFrequency(list.toList))
//
//   }
//
//   def testImplications () = {
//     // Manual check against for size 4 against166 [State 822] TestCaseGenerator: v0___symfile____tmp_input___0_1_symfile___0 = {0x28, 0x35, 0x20, 0x29}; (int32_t) 689976616, (string) "(5 )"
//     // val is = new java.io.FileReader("rebitvec.smt2")
//     val is = new java.io.FileReader("../grammar-learning-dump/outs/no-eval-32-static/size-4/constraints-822.smt2")
//     val lexer = new smtlib.lexer.Lexer(is)
//     val parser = new smtlib.parser.Parser(lexer)
//
//     var cmd = parser.parseCommand
//
//     val fw = new java.io.FileWriter("testImplications.smt2")
//
//     while (cmd != null) {
//       cmd match {
//         case Assert (term) => {
//           println(term.getClass)
//           val i = buildImplication(term)
//           fw.write (Assert (i).toString)
//         }
//         case _ => fw.write(cmd.toString)
//       }
//       cmd = parser.parseCommand
//     }
//
//     fw.close
//   }
//
// }
