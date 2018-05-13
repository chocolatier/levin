package levin

import cats._

// There has to be a better way than defining def show(t: T)
// Right now using either of them requires calling ob.show(ob)
// which is redundant.

sealed trait Gram extends Show[Gram] {
    def show(t: Gram): String = {
        t match {
            case AlternativeG (alternatives) => "  " + alternatives.map(show).mkString("\n  | ")
            case SequenceG (sequence) => sequence.map(show).mkString(" ")
            case LoopG (loopVar) => '(' + show(loopVar) + ')'
            case OptionalG (option) => '[' + show(option) + ']'
            case NameG (name) => name
        }
    }
}

// Apparently the way to define an ADT...............................
// I will be accessing the terminals often enough that it makes
// more sense to put it in a map
case class AlternativeG (alternatives: Set[Gram]) extends Gram
case class SequenceG (sequence: Seq[Gram]) extends Gram
case class OptionalG (option: Gram) extends Gram
case class LoopG (expr: Gram) extends Gram
case class NameG (name: String) extends Gram

case class Grammar(exprMap: Map[String, Gram], terminalMap: Map[String, List[Tuple2[Int, Int]]])  extends Show[Grammar] {
    def show(g: Grammar) : String = {
        g.exprMap.map({case (k,v) => k + " = \n  " + v.show(v)}).mkString("\n") + "\n\n" +
        g.terminalMap.map({case (k,v) => k + " = " + listToASCII(v)}).mkString ("\n")
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

}
