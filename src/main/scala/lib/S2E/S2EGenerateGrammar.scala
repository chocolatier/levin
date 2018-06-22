package levin

import sys.process._
import scala.language.postfixOps

import java.nio.file._

import levin.S2EBootstrap._
import levin.S2EConfig._

import levin.GrammarInference._
import levin.IteratedBruteforce._

object S2EGenerateGrammar {

  def writeFile(path : String, contents : String) {
    val fw = new java.io.FileWriter(path)
    fw.write(contents)
    fw.close
      }

  def learningLoop(length : Int, maxLength : Int ,config : S2EConfig = parseYAMLConfig("./src/main/resources/config.yml")) = {
    val initLoc = "./cache/" + levinConf.Executable + "/init-" + length.toString + "/"
    if (Files.exists(Paths.get(initLoc))) {
      println ("Cached results for execution found. Skipping")
    } else {
      Files.createDirectories(Paths.get(initLoc))
      init(length, config)
      kqueryToSMT2(length, "./cache/" + levinConf.Executable + "/init-" + length.toString + "/")
    }

    val ig = generateInitialGrammar("./cache/" + levinConf.Executable + "/init-" + length.toString + "/")

    for (i <- length+1 to maxLength){
      val iterLoc = "./cache/" + levinConf.Executable + "/iter-" + i.toString + "/"

      if (Files.exists(Paths.get(iterLoc))) {
        println ("Cached results for execution found. Skipping")
      } else {
        Files.createDirectories(Paths.get(iterLoc))
        val disj = disjunctTermsByPerm(ig)
        val newG =  runS2E(i, config, disj.toSeq(0))
      }
    }
  }

  // Does the first run
  def init(length : Int, config : S2EConfig = parseYAMLConfig("./src/main/resources/config.yml")) {
    // Maybe include the S2E Config path in main config?
    val configFile = generateConfigFile(config)
    writeFile(levinConf.ProjectLocation + "s2e-config.lua",configFile)

    val bootstrap = generateBootstrap(length, levinConf.Executable)
    writeFile(levinConf.ProjectLocation + "bootstrap.sh", bootstrap)

    val s = sys.process.Process(Seq("./launch-s2e.sh"), new java.io.File(levinConf.ProjectLocation))!;
  }

  /** Converts the kquery files to SMT2
   */
  def kqueryToSMT2(length : Int, writedir: String ,s2eoutdir : String = "s2e-last/") {

    println("Converting kq to kquery")
    sys.process.Process(Seq("python", "./src/main/misc_scripts/to_kquery.py", levinConf.ProjectLocation + s2eoutdir, length.toString, levinConf.ProjectLocation + s2eoutdir + "all-queries.kquery"))!;

    new java.io.File(writedir).mkdirs

    val kqueryDir = Paths.get(levinConf.ProjectLocation + s2eoutdir)

    // XXX: GROSS
    // Gets all the kquery files
    println("Converting kquery to SMT2")
    for (file <- kqueryDir.toFile.listFiles.filter {case x => (x.getName.dropWhile(_ != '.') == ".kquery") && (x.getName != "all-queries.kquery")} ) {
      val smt2 = (levinConf.Kleaver + " -print-smtlib " + file.getCanonicalPath).!!
      writeFile(writedir + file.getName + ".smt2", smt2)
    }

  }
}
