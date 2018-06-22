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
}
