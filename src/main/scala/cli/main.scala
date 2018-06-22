package levin

import levin.GrammarInference._
import levin.GrammarMutator._

import levin.IteratedBruteforce._

import levin.S2EInputGenerator._
import levin.S2EConfig._
import levin.S2EGenerateGrammar._

import levin.LStar._

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
        levinConf.ProjectLocation = lC.projectLocation
        levinConf.Executable = lC.executable
        levinConf.Kleaver = lC.kleaver
      }
      case Left(_) => throw new Exception("Malformed Configuration File")
    }

    // init(3)

    // kqueryToSMT2(3, "./cache/" + levinConf.Executable + "/init-3/")

    val v = parseYAMLConfig("./src/main/resources/config.yml")
    learningLoop(3,4,v)
    // // val cfg = (generateConfigFile(v))
    // val cfgu = updatePluginsConfig(v.plugins, "AddEqualityRestriction", "eqVector", "{97, 98, 99}")
    //
    // val cfg = generateConfigFile(v.copy(plugins = cfgu))
    //
    // val fw = new java.io.FileWriter(levinConf.ProjectLocation + "s2e-config.lua")
    // println (cfg)
    // fw.write(cfg)
    // fw.close


    val grammarVector = buildGrammarVec("./cache/" + levinConf.Executable + "/init-3/")

    println(grammarVector)
    // Testing Garbage
    // val ig = generateInitialGrammar("./cache/" + levinConf.Executable + "/init-3/")
    // val s = lStar(ig.terminalMap)

    // val cT = ig.exprMap.values.map(classifyTerms)
    // println(cT)
    //
    // val exprSize = {(x : Gram) => x match {case AlternativeG(x) => x.size}}
    //
    //
    // val disj = disjunctTermsByPerm(ig)
    // val newG =  runS2E(4, v, disj.toSeq(0))
    // val disjNewG = disjunctTermsByPerm(newG)
    //
    // println(exprSize(newG.exprMap("Expr")))
    // println(exprSize(disjNewG.toSeq(0).exprMap("Expr")))
    // println (disjNewG.toSeq(0).show(disjNewG.toSeq(0)))

    // val newG2 = runS2E(4, v, disjNewG.toSeq(0))
    // val disjNewG2 = disjunctTermsByPerm(newG2)
    // println (disjNewG2.toSeq(0).show(disjNewG2.toSeq(0)))
    //
    // println (disj.toSeq(0).show(disj.toSeq(0)))
    //
    // println(generateSentences(disj.toSeq(0).exprMap("Expr")).map(concretiseSentence(_, disj.toSeq(0))))
    //
    // generateInputsForGrammar(disj.toSeq(0), "Expr")


  }
}
