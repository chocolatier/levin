package levin

case class LevinConfig(
  smt2ConstraintDir: String,
  binaryLocation: String,
  testFile: String = "testFile"
)

package object levin  {
  var SMT2ConstraintDir = ""
  var BinaryLocation = ""
  var TestFile = ""
}
