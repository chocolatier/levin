package levin

import levin.GrammarInference._
import levin.GrammarMutator._

object cli {
  def main(args: Array[String]) : Unit = {

    // Load configuration details
    // TODO: Figure out how to make config globally available in immutable form
    val config = pureconfig.loadConfig[LevinConfig]
    config match {
      case Right(lC) => {
        levinConf.SMT2ConstraintDir = lC.smt2ConstraintDir
        levinConf.BinaryLocation = lC.binaryLocation
        levinConf.TestFile = lC.testFile
      }
      case Left(_) => throw new Exception("Malformed Configuration File")
    }


    // Testing Garbage
    val ig = generateInitialGrammar()
    val cT = ig.exprMap.values.map(classifyTerms)

    println(disjunctTermsByPerm(ig))

  }
}
