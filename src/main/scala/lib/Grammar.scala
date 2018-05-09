package levin

sealed trait Gram

// Apparently the way to define an ADT...............................
// Read as data Gram = AlternativeT [String] | SequenceT [String] | AlternativeG [Gram] | SequenceG [Gram]
// TODO: Incorporate optional components of grammar.
case class AlternativeT(alternatives: Seq[String]) extends Gram
case class SequenceT(sequence: Seq[String]) extends Gram
case class AlternativeG(alternatives: Seq[Gram]) extends Gram
case class SequenceG(sequence: Seq[Gram]) extends Gram

case class Grammar(expr : Gram, terminalSeq: Seq[String], terminalMap: Map[String, List[Tuple2[Int, Int]]])
