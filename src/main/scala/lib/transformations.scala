package levin

import smtlib._
import scala.collection.mutable.ListBuffer

package object Transformations {
  // Based on example in the scala-smtlib readme
  def toCmdList(x: smtlib.parser.Parser) = {
    var cmds = new ListBuffer[smtlib.trees.Commands.Command]
    var cmd = x.parseCommand

    while(cmd != null){
      cmds.append(cmd)
      cmd = x.parseCommand
    }
    cmds.toList

  }

}
