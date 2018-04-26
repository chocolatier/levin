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

import play.api.libs.json._

// Example usage of levin. (Actually just quick and dirty testing).
object Example {
  def main (args: Array[String]): Unit = {
    // testGetCommonPatterns("../grammar-learning-dump/outs/no-eval-32-static/size-4")
    // testImplications
    val v = iterateOverBitvec("../grammar-learning-dump/outs/no-eval-32-static/size-4", 4)
    println(v)
    val x = Json.toJson(v)

    val fw = new java.io.FileWriter("constraintMapping.json")
    println(x)
    fw.write(x.toString)
    fw.close

  }

  def iterateOverBitvec (path : String, length : Int) = {
    val files  = new File(path).listFiles.filter(_.getName.endsWith(".smt2"))
    var list = scala.collection.mutable.ListBuffer.empty[Seq[Term]]

    var stateConstList = scala.collection.mutable.ListBuffer.empty[(String,Map[Int, List[Tuple2[Int, Int]]])]

    for (file <- files) {
      var ctx = scala.collection.mutable.ListBuffer.empty[Command]
      println(file)
      val is = new java.io.FileReader(file)
      val lexer = new smtlib.lexer.Lexer(is)
      val parser = new smtlib.parser.Parser(lexer)

      var cmd = parser.parseCommand
      while (cmd != null) {
        cmd match {
          case DeclareFun (f, g, h) => {
            ctx += cmd
            cmd = parser.parseCommand
            cmd match {
              case Assert (x) => {
                var stateTypeMap = new scala.collection.mutable.HashMap[Int, List[Tuple2[Int, Int]]]
                for (i <- 0 to (length - 1)){
                  val m = inferType(buildbvSelect(f,i), x, ctx)
                  stateTypeMap += (i -> m)
                  }
                  stateConstList += new Tuple2(file.getName, stateTypeMap.toMap)
                }
              case _ => //println(cmd)
            }
          }
          case Assert (x) => // println("asdf")
          case _ => ctx += cmd
        }
        cmd = parser.parseCommand
      }
    }
    stateConstList
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


  def buildbvSelect (identifier : SSymbol , position: Int) = {
    Select(QualifiedIdentifier(SimpleIdentifier(identifier)), QualifiedIdentifier(Identifier(SSymbol("bv" + position.toString),List(SNumeral(32))), None))

  }

  def buildAnd (t: Term) = {
    val letFcn = createLetFcn (t)
    val woLets = stripLets (t)
    val app = grabFirstLet (t)
    letFcn (buildFunctionApplication("and", Seq(ctypeSMTGen(app, "garbage"), woLets)))
  }

  def buildImplication (t : Term) = {
    val letFcn = createLetFcn (t)
    val woLets = stripLets (t)
    val app = grabFirstLet (t)
    letFcn (Implies (ctypeSMTGen(app, "garbage"), woLets))

  }

  def grabFirstLet (t : Term) = {
    t match {
      case Let (VarBinding(_,v), _, _) => {
        v
        }
      case _ => t //TODO: Handle error
    }
  }

}
