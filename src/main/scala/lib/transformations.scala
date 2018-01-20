package levin

import smtlib._
import smtlib.trees.Commands._
import smtlib.trees.Terms._

import scala.collection.mutable.ListBuffer

package object Transformations {

    /**
    * Splits an expression into a series of expressions.
    * TODO: Return a valid list of expressions.
    */
    def andToVec (x: smtlib.parser.Parser) = {
      var common = new ListBuffer[smtlib.trees.Commands.Command]
      var toSplit = new ListBuffer[smtlib.trees.Commands.Command]
      var cmd = x.parseCommand
      cmd.getClass.getMethods.map(_.getName)
      while (cmd != null){
        cmd match {
          case Assert (term) => createDisjointAssertions(term)
          case _ => common.append(cmd)
        }
        cmd = x.parseCommand
      }
      common
    }

    def createDisjointAssertions (expr : SExpr) : Seq[SExpr] = {
      expr match {
        // XXX: Just stripping the declaration away. Will need to be fixed
        // if the disjoint list of assertions is to be used in any way.
        case Let (vars, seqVars, t) => createDisjointAssertions(t)
        case FunctionApplication(QualifiedIdentifier(Identifier(SSymbol("and"),_),_), terms) => {
          terms.flatMap(createDisjointAssertions)
        }
        case x => {
          x::Nil}
      }
    }
}
