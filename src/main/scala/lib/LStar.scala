package levin

import levin.GrammarMutator._

// Implementation of LStar as found at
// http://pagesperso.lina.univ-nantes.fr/~cdlh//book/Learning_with_Queries.pdf

object LStar {

  sealed trait STA
  case class Red (s: Sentence) extends STA
  case class Blue (s : Sentence) extends STA

  type Sentence = List[String]

  // var sta = Set.empty : Set[STA]
  // var exp = Set.empty : Set[Sentence]
  // var red = Set.empty : Set[Red]
  // var blue = Set.empty : Set[Blue]

  case class Table (observation_cache: Map[String, Int], experiment : List[Sentence])

  def lStarInitialise(terminalMap: Map[String, List[Tuple2[Int, Int]]]) : Table = {
    var red = Set(Red(List()))
    var blue = terminalMap.keys.map{case x => Blue(List(x))}.toSet

  }

  def OT (u : Sentence, e : Sentence, terminalMap : Map[String, List[Tuple2[Int, Int]]]) : Boolean = {
    val newSeq = SequenceG(((u ++ e).map {case x => NameG(x)}))
    val subGram = Grammar (Map("Expr" -> newSeq), terminalMap)

    val g = testGrammar(subGram)

    return g
  }

  def isLStarClosed(table : Table) : Boolean = {
    return true
  }

  def isLStarConsistent(table: Table) : Boolean = {
    return true
  }

  def lStarUseEQ (table : Table, answer : Boolean) : Table = {
    return table
  }

  def lStarEq (table : Table) : Boolean = {
    return false
  }

  def lStarClose (table : Table) : Table = {
    return table
  }

  def lStarConsistent(table : Table) : Table = {
    table
  }

  def lStar(terminalMap: Map[String, List[Tuple2[Int, Int]]]) = {
    var answer = false
    var table = lStarInitialise(terminalMap)
    do  {
      while (!isLStarConsistent(table) || !isLStarClosed(table)){
        table = lStarClose(table)
        if (!isLStarConsistent(table)){
          table = lStarConsistent(table)
        }
      }
      answer = lStarEq(table)
      if (!answer){
        table = lStarUseEQ(table, answer)
      }
    } while (!answer)
  }
}
