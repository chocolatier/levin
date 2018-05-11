package levin

object cli {
  def main(args: Array[String]) : Unit = {
    val config = pureconfig.loadConfig[LevinConfig]
    config match {
      case Right(lC) => {
        levin.SMT2ConstraintDir = lC.smt2ConstraintDir
        levin.BinaryLocation = lC.binaryLocation
        levin.TestFile = lC.testFile
      }
      case Left(_) => throw new Exception("Malformed Configuration File")
    }

  }
}
