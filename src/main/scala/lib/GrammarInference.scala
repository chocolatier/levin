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

import cats._
import cats.Semigroup
import cats.implicits._

object GrammarInference {
  var t = new scala.collection.mutable.HashMap[List[Tuple2[Int, Int]], String]

  // TODO: Write printer for modular funcs, remove.
  def mapToGrammar (m : Map[Int, List[Tuple2[Int, Int]]]) = {
    val seq = m.toSeq.sortBy(_._1)

    var gram = ""

    for (g <- seq){
        gram += getName(g._2)
    }
    gram
  }

  def mapToSequenceG (m : Map[Int, List[Tuple2[Int, Int]]]) = {
    val seq = m.toSeq.sortBy(_._1)
    SequenceG(seq.map(g => NameG(getName(g._2))))
  }

  def mapToAlternativeG (m : List[Map[Int, List[Tuple2[Int, Int]]]]) = {
    AlternativeG(m.map(mapToSequenceG).toSet)
  }

  def generateInitialGrammar (path : String) = {
    val expr = mapToAlternativeG(buildGrammarVec(path).map(_._2).toList)
    val tM = t.map(_.swap).toMap
    Grammar(Map("Expr" -> expr), tM)
  }

  // For now just working on top level AlternativeG[SequenceT]
  def classifyTerms (g : Gram) = {
    println ("classifing terms")
    var edgeMap = Map[String, Tuple2[Int, Int]]()
    g match {
      case AlternativeG (alts) => {
          for (alt <- alts) {
            alt match {
              case SequenceG (Seq(NameG(_))) => {} // No point in checking single elem sequences
              case SequenceG (sq) => {
                edgeMap = edgeMap combine countEdges(sq)
              }
              case _ => throw new NotImplementedError()
            }
          }
      }
      case _ => throw new NotImplementedError ()
    }
    edgeMap.groupBy(_._2).mapValues(_.keys).values
  }


  // s needs to have atleast 2 elements or sliding throws an error
  def countEdges (s : Seq[Gram]) = {
    val tups = s.sliding(2).map ({ case Seq(NameG(a), NameG(b)) => (a, b) })
    var edgeMap = new scala.collection.mutable.HashMap[String, Tuple2[Int, Int]]
    for (tup <- tups) {
        edgeMap += (tup._1 -> ((edgeMap.get(tup._1).getOrElse((0,0))) combine (1,0)))
        edgeMap += (tup._2 -> ((edgeMap.get(tup._2).getOrElse((0,0))) combine (0,1)))
    }
    edgeMap.toMap
  }

  // def disjunctTermsByPerm (g : Grammar) = {
  //   val potentialDisjunctables = classifyTerms(g).filter(_.size > 1)
  //   potentialDisjunctables.map(sieveTerms(g, _))
  //
  // }

  // def sieveTerms(g : Grammar, terms: Iterable[String]) = {
  //   var terms2 = terms
  //   for (t0 <- terms2) {
  //       val equals = t0::terms2.tail.map(substituteTerms(g, t0,_)).filter(isEqualGramamr)
  //       terms2 = terms2 \\ equals
  //   }
  // }

  def substituteTerms (g : Gram, t0: String, t1: String) : Gram = {
    g match {
      case NameG(name) => NameG (name match {case `t0` => t1; case `t1` => t0; case x => x})
      case AlternativeG(grams) => AlternativeG (grams.map {x => substituteTerms(x, t0, t1)})
      case SequenceG(grams) => SequenceG (grams.map {x => substituteTerms(x, t0, t1)})
      case LoopG (gram) => LoopG (substituteTerms(gram, t0, t1))
      case OptionalG (gram) => OptionalG (substituteTerms(gram, t0, t1))

    }
  }

  def getName (f : List[Tuple2[Int, Int]]) = {
    val sorted_f = f.sorted
    t.get(sorted_f).getOrElse(generateNewName(sorted_f)) + " "
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

  def simplifyGrammarVec (stateConstList : List[(Int,Map[Int, List[Tuple2[Int, Int]]])], disjunctableStates : List[List[Int]] ) = {
      val stateConstMap = stateConstList.toMap

      var simplifiedConstList = scala.collection.mutable.ListBuffer.empty[Map[Int, List[Tuple2[Int, Int]]]]

      for (disjStates <- disjunctableStates){
        val disjunctableMaps = stateConstMap.filterKeys(disjStates.contains(_)).values
        simplifiedConstList += disjunctableMaps.reduceLeft(mergeStateTypeMaps)
      }

      val disjunctableStateList = disjunctableStates.flatten
      val nonDisjStates = stateConstMap.filterKeys(!disjunctableStateList.contains(_)).values

      for (v <- nonDisjStates){
        simplifiedConstList += v
      }

      simplifiedConstList
  }

  def mergeStateTypeMaps (a : Map[Int, List[Tuple2[Int, Int]]], b : Map[Int, List[Tuple2[Int, Int]]]) = {
    a ++ b.map {
      case (id, xs) => id -> (xs.union(a.getOrElse(id, List[Tuple2[Int,Int]]())).distinct)
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
