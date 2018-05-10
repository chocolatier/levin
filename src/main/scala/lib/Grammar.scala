package levin

sealed trait Gram

// Apparently the way to define an ADT...............................
// I will be accessing the terminals often enough that it makes
// more sense to put it in a map
case class AlternativeG (alternatives: Set[Gram]) extends Gram
case class SequenceG (sequence: Seq[Gram]) extends Gram
case class OptionalG (option: Gram) extends Gram
case class LoopG (expr: Gram) extends Gram
case class NameG (name: String) extends Gram

case class Grammar(exprMap: Map[String, Gram], terminalMap: Map[String, List[Tuple2[Int, Int]]])
