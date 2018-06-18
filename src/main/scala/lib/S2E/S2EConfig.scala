package levin

object S2EConfig {
  case class LoggingOptions(console : String, logLevel : String)
  case class S2EOptions(logging : LoggingOptions, kleeArgs : List[String])

  sealed trait ConfigObject
  case class ConfigString (s : String) extends ConfigObject
  case class PluginConfig (name : String, value : ConfigObject) extends ConfigObject
  case class PluginOptions (name : String, config : List[PluginConfig])

  case class S2EConfig(s2e : S2EOptions, plugins : PluginOptions)

  def parseYAMLConfig(path : String) = {

  }

  // def generateConfigFile
}
