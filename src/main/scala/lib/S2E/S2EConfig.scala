package levin

import scala.io.Source

import cats.syntax.either._

import io.circe.yaml.parser
import io.circe.generic.auto._
import io.circe._

object S2EConfig {
  case class LoggingOptions(console : String, logLevel : String)
  case class S2EOptions(logging : LoggingOptions , kleeArgs : List[String])

  // sealed trait ConfigObject
  // case class ConfigString (s : String) extends ConfigObject
  case class PluginConfig (name : String, value : String) // extends ConfigObject
  case class PluginOptions (name : String, config : List[PluginConfig])

  case class S2EConfig(s2e : S2EOptions, plugins : List[PluginOptions])

  def parseYAMLConfig(path : String) = {
      val config = Source.fromFile(path).getLines.mkString("\n")
      val json  = yaml.parser.parse(config)

      val s2econfig = json
          .leftMap(err => err: Error)
          .flatMap(_.as[S2EConfig])
          .valueOr(throw _) : S2EConfig
    s2econfig
  }

  def generateConfigFile (config : S2EConfig) = {
    val s2eString = "s2e = {" + showLogging(config.s2e.logging) +
      "," + showkleeArgs(config.s2e.kleeArgs) +  "}"

    val convenient = """
    plugins = {}
    pluginsConfig = {}

    dofile('library.lua')
            """

    val plugins = showPlugins(config.plugins)

    List(s2eString, convenient, plugins).mkString("\n\n")

  }

  def showLogging (logging : LoggingOptions) = {
    ("logging = {"
      + "\nconsole = " + logging.console
      + ",\nlogLevel = " + logging.logLevel
      + "\n}")
  }

  def showkleeArgs(kleeArgs : List[String]) = {
    ("kleeArgs = {\n"
      + kleeArgs.map {case x => "\"--" + x + "\""}.mkString(",\n")
      + "\n}")
  }

  def showPlugins(plugins : List[PluginOptions]) = {
    plugins.map(showPlugin(_)).mkString("\n")
  }

  def showPlugin(plugin : PluginOptions) = {
    val configString =
      if (plugin.config.length > 0) {
        ("pluginsConfig." + plugin.name + " = {\n"
        +  plugin.config.map {showConfigObject(_)}.mkString(",\n")
        + "}")
      } else {
        ""
      }
    ("add_plugin(\"" + plugin.name + "\")\n" + configString
    )
  }

  def showConfigObject(c : PluginConfig) : String = {
    (c.name + " = " + c.value)
    // c match {
    //   case PluginConfig(name, ConfigString(s)) =>  name + "=" + s
    //   case PluginConfig(name, conf@PluginConfig(_,_)) => name + " = {\n" + showConfigObject(conf) + "\n}"
    // }

  }

  def updatePluginsConfig(plugins : List[PluginOptions], targetPlugin : String, updateName : String, updateValue : String) = {
    val remainingConfigs = plugins.filter(_.name != targetPlugin)
    val targetConfig = plugins.filter(_.name == targetPlugin)(0)

    val remainingValues = targetConfig.config.filter(_.name != updateName)
    val updatedName = PluginConfig(updateName,updateValue)::remainingValues
    val update = PluginOptions(targetPlugin, updatedName)

    (update) :: remainingConfigs
  }
}
