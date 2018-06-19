package levin

import levin.S2EBootstrap._
import levin.S2EConfig._

object S2EGenerateGrammar {

  // Does the first run
  def init(length : Int) {
    // Maybe include the S2E Config path in main config?
    val config = parseYAMLConfig("./src/main/resources/config.yml")
    val configFile = generateConfigFile(config)

    val bootstrap = generateBootstrap(length, levinConf.Executable)

    val fw = new java.io.FileWriter(levinConf.ProjectLocation + "s2e-config.lua")
    fw.write(configFile)
    fw.close

    val bsfw = new java.io.FileWriter(levinConf.ProjectLocation + "bootstrap.sh")
    bsfw.write(bootstrap)
    bsfw.close

  }
}
