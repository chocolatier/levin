package levin

object cli {
  def main(args: Array[String]) : Unit = {
    val config = pureconfig.loadConfig[LevinConfig]
    println(config)
  }
}
