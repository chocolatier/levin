package levin.automata

object Automaton {

  case class State(name: String, transitionMap : Map[Tuple2[StackLetter, String], String])
  case class StackLetter(name: String)

  /** Represents a Deterministic Push Down Automaton.
    */
  case class Automaton (states : Map[String, State]) {
    def update(state : State) = {
      Automaton(this.states + (state.name -> state))
    }
  }

  /** Builds an acceptor automaton based on the sentences it recieves as an input
    * Params : sentences: The sentences we are building the acceptor for.
    */
  def buildInitialAutomaton(sentences : Set[List[String]]) : Automaton = {
    val blankLetter = StackLetter("Initial")
    var currentAutomaton = Automaton(Map("Start" -> State("Start", Map())))

    for (sentence <- sentences){
      var currentState = currentAutomaton.states("Start")
      for (word <- sentence) {
        val updatedState = currentState.copy(transitionMap = currentState.transitionMap + ((blankLetter, word) -> word))
        currentAutomaton = currentAutomaton.update(updatedState)
        currentState = lookupState(word, currentAutomaton)
      }
    }
    currentAutomaton
  }

  /** Looks up the state in an automaton or constructs a new state
    * If it is not available
    */
  def lookupState(name : String, automaton: Automaton) : State = {
    automaton.states.get(name).getOrElse(State(name, Map()))
  }

  /** Collects the nodes which lead into a particular state
    */
  def inEdges(a : Automaton, s : State) : List[State] = {
    a.states.values.filter(_.transitionMap.values.toList contains s.name).toList
  }

  /* Collects the nodes a state can lead to. */
  def outEdges(a : Automaton, s : State) : List[State] = {
    s.transitionMap.values.map(lookupState(_, a)).toList
  }

  /* Groups the nodes of the automaton
   */
  def groupStates (a : Automaton) = {
    val numEdges = a.states.map {case x => (x._2, (inEdges(a,x._2), outEdges(a, x._2)))}
    numEdges.groupBy(_._2)
  }

  /* Combines two states in the automaton */
  def combineStates (a : Automaton, s1 : State, s2: State) = {
    if (s1.transitionMap.values != s2.transitionMap.values) {
      println("[Warning]: Combining states where $s1.name != $s2.name. Using $s1.name transitionMap")
    }

    val newName = s1.name + "," + s2.name
    val combinedState = (newName, s1.transitionMap)
    val states = a.states - s1.name - s2.name

    val updatedStates = states.map {
      case (k,v) => (k -> updateState(v, s1, s2, newName))
    }
  }

  // Replaces transitions to the old states with transition to the new one.
  def updateState(s: State, s1: State, s2: State, newName: String) = {
    s.transitionMap.map {
      case (k, v) => (k -> nameReplace(s1.name, s2.name, newName, s.name))
    }
  }

  def nameReplace (t0: String, t1: String, n : String, in: String) = {
    in match {
      case `t0` => n
      case `t1` => n
      case x => x
    }
  }

}
