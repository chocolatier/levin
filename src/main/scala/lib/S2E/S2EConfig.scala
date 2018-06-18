package levin

import scala.io.Source

import cats.syntax.either._

import io.circe.yaml.parser
import io.circe.generic.auto._
import io.circe._

object S2EConfig {
  case class LoggingOptions(console : String, logLevel : String)
  case class S2EOptions(logging : LoggingOptions, kleeArgs : List[String])

  sealed trait ConfigObject
  case class ConfigString (s : String) extends ConfigObject
  case class PluginConfig (name : String, value : ConfigObject) extends ConfigObject
  case class PluginOptions (name : String, config : List[PluginConfig])

  case class S2EConfig(s2e : S2EOptions, plugins : List[PluginOptions])

  def parseYAMLConfig(path : String) = {
      val config = Source.fromFile(path).getLines.mkString
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

    val convenient = """plugins = {}
            pluginsConfig = {}

            dofile('library.lua')
            """

    val plugins = showPlugins(config.plugins)

  }

  def showLogging (logging : LoggingOptions) = {
    ("logging = {"
      + "\nconsole = " + logging.console
      + "\nlogLevel = " + logging.logLevel
      + "\n}")
  }

  def showkleeArgs(kleeArgs : List[String]) = {
    ("kleeArgs = {\n"
      + kleeArgs.map {case x => "--\"" + x + "\""}.mkString(",\n")
      + "\n}")
  }

  def showPlugins(plugins : List[PluginOptions]) = {
    plugins.map(showPlugin(_)).mkString("\n")
  }

  def showPlugin(plugin : PluginOptions) = {
    ("add_plugin(" + plugin.name + ")\n"
      + "pluginsConfig." + plugin.name + "= {"
      +  plugin.config.map {showConfigObject(_)}.mkString(",\n")
    )
  }

  def showConfigObject(c : PluginConfig) : String = {
    c match {
      case PluginConfig(name, ConfigString(s)) =>  name + "=" + s
      case PluginConfig(name, conf@PluginConfig(_,_)) => name + " = {\n" + showConfigObject(conf) + "\n}"
    }

  }
}
