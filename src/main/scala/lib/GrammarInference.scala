package levin

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


object GrammarInference {
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

    var stateConstList = scala.collection.mutable.ListBuffer.empty[(Int,Map[Int, List[Tuple2[Int, Int]]])]

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
    Tuple2(constraintNameToID(file.getName), stateTypeMap.toMap)
  }

  def constraintNameToID(name : String) = {
    // Regex stolen from https://stackoverflow.com/questions/4545937/java-splitting-the-filename-into-a-base-and-extension
    name.split("\\.(?=[^\\.]+$)")(0).filter(_.isDigit).toInt
  }

}
