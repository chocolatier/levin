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
    val bvc = buildGrammarVec("../grammar-constraint-analysis/constraints/smt2/")
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

  var t = new scala.collection.mutable.HashMap[List[Tuple2[Int, Int]], String]

  def mapToGrammar (m : Map[Int, List[Tuple2[Int, Int]]]) = {
    val seq = m.toSeq.sortBy(_._1)

    var gram = ""

    for (g <- seq){
        gram += t.get(g._2).getOrElse(generateNewName(g._2)) + " "
    }
    gram
  }

  def generateNewName(f: List[Tuple2[Int, Int]]) = {
    val taken = t.values.toList.sorted
    // println(taken)
    if (taken.isEmpty){
      t += (f -> "g0")
      "g0"
    } else {
      val n = taken.last.filter(_.isDigit).toInt
      val newname = "g" + (n + 1).toString
      t += (f -> newname)
      newname
    }
  }

  def listToASCII (l : List[Tuple2[Int, Int]]) = {
    l.map(intTupToStr).mkString(" | ")
  }

  def intTupToStr(it : Tuple2[Int,Int]) = {
    if (it._1 == it._2){
          if (it._1 == 0) {
            "'\\0'"
          } else {
            "'" ++ it._1.toChar.toString  ++ "'"
          }
    } else {
        "'" ++ ((it._1).toChar).toString ++ "' - '" ++ ((it._2).toChar).toString ++ "'"
    }
  }

  def buildGrammarVec (path : String) = {
    val files  = new File(path).listFiles.filter(_.getName.endsWith(".smt2"))
    var list = scala.collection.mutable.ListBuffer.empty[Seq[Term]]

    var stateConstList = scala.collection.mutable.ListBuffer.empty[(String,Map[Int, List[Tuple2[Int, Int]]])]

    for (file <- files) {
      println("processing " + file)
      stateConstList += constructStateTypeMap(file)
    }
    stateConstList
  }

  def constructStateTypeMap (file : File) = {

    var ctx = scala.collection.mutable.ListBuffer.empty[Command]
    val is = new java.io.FileReader(file)
    val lexer = new smtlib.lexer.Lexer(is)
    val parser = new smtlib.parser.Parser(lexer)

    var cmd = parser.parseCommand

    var stateTypeMap = new scala.collection.mutable.HashMap[Int, List[Tuple2[Int, Int]]]

    while (cmd != null) {
      cmd match {
        case DeclareFun (f, g, h) => {
          ctx += cmd
          cmd = parser.parseCommand
          cmd match {
            case Assert (x) => {

              val maxIndex =
                if (file.getName contains "constraints-0.smt2"){
                  0
                } else {
                  inferMaxIndex(x)
                }

              for (i <- 0 to maxIndex) {
                val m = inferType(buildbvSelect(f,i), x, ctx)
                stateTypeMap += (i -> m)
                }
              }
            case _ => {}
          }
        }
        case _ => ctx += cmd
      }
      cmd = parser.parseCommand
    }
    Tuple2(file.getName, stateTypeMap.toMap)
  }

  // Assumption: Length is there within a let statement
  // Returns the length - 1
  def inferMaxIndex(t : Term) = {
    listLets(t).map(lengthFromVarBinding).max
  }

  def lengthFromVarBinding (vb : VarBinding) = {
    getIndexFromSelect (vb.term)
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
