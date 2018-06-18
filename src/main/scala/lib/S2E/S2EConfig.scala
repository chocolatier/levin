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

  case class S2EConfig(s2e : S2EOptions, plugins : PluginOptions)

  def parseYAMLConfig(path : String) = {
      val config = Source.fromFile(path).getLines.mkString
      val json  = yaml.parser.parse(config)

      val s2econfig = json
          .leftMap(err => err: Error)
          .flatMap(_.as[S2EConfig])
          .valueOr(throw _) : S2EConfig
    s2econfig
  }

  // def generateConfigFile
}
