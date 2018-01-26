package levin

import smtlib._
import smtlib.trees.Commands._
import smtlib.trees.Terms._

import scala.collection.mutable.ListBuffer

package object Transformations {

    /**
    * Splits an expression into a series of expressions.
    * XXX: Replace this imperetive BS with functional code.
    */
    def andToVec (x: smtlib.parser.Parser) = {
      var common = new ListBuffer[ListBuffer[Command]]
      common.append(new ListBuffer[Command])
      var cmd = x.parseCommand
      while (cmd != null){
        cmd match {
          case Assert (term) => {
            val letFcn = createLetFcn(term)
            val strippedLets = stripLets(term)
            val disjointTerms = createDisjointAssertions (strippedLets)
            for (c <- common){
              common -= c
              for (d <- disjointTerms.map(letFcn).map(Assert)) {
                var e = c.clone
                e.append(d)
                common.append(e)
              }
            }
          }
          case _ => common.foreach(_.append(cmd))
        }
        cmd = x.parseCommand
      }
      common
    }

    def createLetFcn (term: Term) : Term => Term = {
      term match {
        case Let (vars, seqVars, x) => (Let (vars, seqVars, _:Term)) compose (createLetFcn(x))
        case _ => identity
      }

    }

    def stripLets (expr : Term) : Term = {
      expr match {
        case Let (_, _, t) => stripLets (t)
        case _ => expr
      }
    }

    def createDisjointAssertions (expr : Term) : Seq[Term] = {
      expr match {
        // gross
        case FunctionApplication(QualifiedIdentifier(Identifier(SSymbol("and"),_),_), terms) => {
          terms.flatMap(createDisjointAssertions)
        }
        case x => x::Nil
      }
    }
}
