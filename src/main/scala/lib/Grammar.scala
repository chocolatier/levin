package levin

sealed trait Gram

case class AlternativeT(alternatives: Seq[String]) extends Gram
case class SequenceT(sequence: Seq[String]) extends Gram
case class AlternativeG(alternatives: Seq[Gram]) extends Gram
case class SequenceG(sequence: Seq[Gram]) extends Gram

case class Grammar(expr : Gram, terminalSeq: Seq[String], terminalMap: Map[String, List[Tuple2[Int, Int]]])
