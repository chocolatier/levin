package levin

case class LevinConfig(
  smt2ConstraintDir: String,
  binaryLocation: String,
  projectLocation : String,
  executable : String,
  testFile: String = "testFile"
)

package object levinConf  {
  var SMT2ConstraintDir = ""
  var BinaryLocation = ""
  var ProjectLocation = ""
  var Executable = ""
  var TestFile = ""
}
