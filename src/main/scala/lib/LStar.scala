package levin

import levin.GrammarMutator._

// Implementation of LStar as found at
// http://pagesperso.lina.univ-nantes.fr/~cdlh//book/Learning_with_Queries.pdf

object LStar {

  type Sentence = List[String]

  case class Table (red : Set[Sentence], blue : Set[Sentence], observation_cache: Map[Sentence, Boolean], experiment : Set[Sentence], terminalMap : Map[String, List[Tuple2[Int, Int]]])

  def lStarInitialise(terminalMap: Map[String, List[Tuple2[Int, Int]]]) : Table = {
    var red = Set(List()) : Set[Sentence]
    var blue = terminalMap.keys.map{case x => (List(x))}.toSet
    var exp = Set(List()) : Set[Sentence]

    var observation_cache = Map (List() -> membershipQuery(List(), List(), terminalMap)) : Map[Sentence, Boolean]

    for (x <- red) {
      observation_cache += (x -> membershipQuery(x, List(), terminalMap))
    }

    for (x <- blue) {
      observation_cache += (x -> membershipQuery(x, List(), terminalMap))
    }

    Table (red, blue, observation_cache, exp, terminalMap)

  }

  def membershipQuery (u : Sentence, e : Sentence, terminalMap : Map[String, List[Tuple2[Int, Int]]]) : Boolean = {
    val newSeq = SequenceG(((u ++ e).map {case x => NameG(x)}))
    val subGram = Grammar (Map("Expr" -> newSeq), terminalMap)

    val g = testGrammar(subGram)

    return g
  }

  def isLStarClosed(table : Table) : Boolean = {
    for (x <- table.red) {
      for (e <- table.experiment) {
        if (!table.observation_cache.contains(x ++ e)){
          return false;
        }
      }
    }

    for (x <- table.blue) {
      for (e <- table.experiment) {
        if (!table.observation_cache.contains(x ++ e)){
          return false;
        }
      }
    }

    return true
  }

  def lookupTableRow (s : Sentence, t : Table) : Map[Sentence, Boolean] = {
    t.observation_cache.filterKeys(_.take(s.length) == s)
  }

  def tableRowsEqual(s1 : Sentence, t1 : Map[Sentence, Boolean], s2: Sentence, t2 : Map[Sentence, Boolean]) : Boolean = {
    for (x <- t1.keySet) {
      if (t1(x) != t2(s2 ++ x.drop(s1.length))) {
        return false
      }
    }
    return true
  }

  def isLStarConsistent(table: Table) : Boolean = {
    // TODO: Optimize
    for (x <- table.red) {
      for (y <- table.red) {
        val xrow = lookupTableRow(x, table)
        val yrow = lookupTableRow(y, table)
        if (tableRowsEqual(x, xrow, y, yrow)){
          for (t <- table.terminalMap.keySet) {
            if (membershipQuery(x, List(t), table.terminalMap) != membershipQuery(y, List(t), table.terminalMap)){
              return false
            }
          }
        }
      }
    }
    return true
  }

  // TODO: LStarEq and LStarUseEQ after linking levin back to S2E
  // General idea: Check if we get 100% coverage by executing
  // every sentence. Otherwise use DSE to explore uncovered branch
  // and generate a counter-example

  def lStarUseEQ (table : Table, answer : Boolean) : Table = {
    return table
  }

  // TODO
  // Checks if the grammar is equivalent
  def lStarEq (table : Table) : Boolean = {
    return false
  }


  def lStarClose (table : Table) : Table = {
    var red = table.red
    var blue = table.blue
    var observation_cache = table.observation_cache
    for (s <- table.blue) {
      val redRows = table.red.map(lookupTableRow(_, table))
      if (!(redRows `contains` lookupTableRow(s, table))) {
        red += s
        blue -=s
        for (a <- table.terminalMap.keySet){
          blue += (s ++ List(a))
        }
      }

      for (x <- table.red) {
        for (e <- table.experiment) {
          if (!observation_cache.contains(x ++ e)) {
            observation_cache += (x ++ e -> membershipQuery(x, e, table.terminalMap))
          }
        }
      }
    }

    return Table(red, blue, observation_cache, table.experiment, table.terminalMap)
  }

  def lStarConsistent(table : Table) : Table = {
    // TODO: Optimize
    var exp = table.experiment
    var observation_cache = table.observation_cache

    for (x <- table.red) {
      for (y <- table.red) {
        if (tableRowsEqual (x, lookupTableRow(x, table), y,lookupTableRow(y, table))){
          for (t <- table.terminalMap.keySet){
            if (membershipQuery(x, List(t), table.terminalMap) != membershipQuery(y, List(t), table.terminalMap)){
              exp += (x ++ List(t))
            }
          }
        }
      }
    }

    for (x <- table.red) {
      for (e <- table.experiment) {
        if (!observation_cache.contains(x ++ e)) {
          observation_cache += (x ++ e -> membershipQuery(x, e, table.terminalMap))
        }
      }
    }

    table.copy(experiment = exp, observation_cache = observation_cache)
  }

  def lStar(terminalMap: Map[String, List[Tuple2[Int, Int]]]) = {
    var answer = false
    var table = lStarInitialise(terminalMap)
    do  {
      println("Consistency:" + isLStarConsistent(table))
      println("Closure:" + isLStarClosed(table))

      while (!isLStarConsistent(table) || !isLStarClosed(table)){
        if (!isLStarClosed(table)){
          table = lStarClose(table)
        }
        if (!isLStarConsistent(table)){
          table = lStarConsistent(table)
        }
      }
      answer = lStarEq(table)
      if (!answer){
        table = lStarUseEQ(table, answer)
      }
      println(table)
    } while (!answer)
  }
}
