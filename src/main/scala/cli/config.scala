package levin

case class LevinConfig(
  smt2ConstraintDir: String,
  binaryLocation: String,
  testFile: String = "testFile"
)
