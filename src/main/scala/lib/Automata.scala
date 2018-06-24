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

  /** Counts the number of nodes that lead into a particular state
    */
  def inEdges(a : Automaton, s : State) : Int = {
    var rv = 0
    for (state <- a.states.values) {
      if (state.transitionMap.values.toList contains s.name) {
        rv += 1
      }
    }
    rv
  }

  /* Counts the number of nodes a state can lead to.
   */
  def outEdges(s : State) = {
    s.transitionMap.size
  }

  /* Groups the nodes of the automaton
   */
  def groupStates (a : Automaton, s : State) = {
    val numEdges = a.states.map {case x => (x, (inEdges(a,s), outEdges(s)))}
    numEdges.groupBy(_._2)
  }
}
