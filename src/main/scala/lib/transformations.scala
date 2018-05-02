package levin

import smtlib._
import smtlib.trees.Commands._
import smtlib.trees.Terms._
import theories.Core._
import trees.Terms._
import theories.ArraysEx._
import theories.Ints.{IntSort, NumeralLit}

import scala.collection.mutable.ListBuffer

import levin.analysis._

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

  // Returns a sequence of terms when it recieves something joined by ands. Will replace andToVec once its other functionality is split nicely. 
    def andToVec2 (t : Term) : Seq[Term] = {
      t match {
        case FunctionApplication (QualifiedIdentifier(Identifier(SSymbol("and"),_),_), terms) => terms.flatMap(andToVec2)
        case _ => Seq(t)
      }
    }

    // Creates an higher order function can be used to 
    // let statements ti a term 
    def createLetFcn (term: Term) : Term => Term = {
      term match {
        case Let (vars, seqVars, x) => (Let (vars, seqVars, _:Term)) compose (createLetFcn(x))
        case _ => identity
      }
    }

   def listLets (term : Term) : List[VarBinding] = {
     term match {
       case Let (vars, seqVars, x) => (vars::seqVars.toList):::listLets(x)
       case _ => List() 
     }
   }

    // Removes the let statements from a term
    def stripLets (expr : Term) : Term = {
      expr match {
        case Let (_, _, t) => stripLets (t)
        case _ => expr
      }
    }

    // Splits a series of ands, forming a sequence of terms
    def createDisjointAssertions (expr : Term) : Seq[Term] = {
      expr match {
        // gross
        case FunctionApplication(QualifiedIdentifier(Identifier(SSymbol("and"),_),_), terms) => {
          terms.flatMap(createDisjointAssertions)
        }
        case x => x::Nil
      }
    }

    
  def buildbvSelect (identifier : SSymbol , position: Int) = {
    Select(QualifiedIdentifier(SimpleIdentifier(identifier)), QualifiedIdentifier(Identifier(SSymbol("bv" + position.toString),List(SNumeral(32))), None))

  }

  def buildAnd (t: Term) = {
    val letFcn = createLetFcn (t)
    val woLets = stripLets (t)
    val app = grabFirstLet (t)
    letFcn (buildFunctionApplication("and", Seq(ctypeSMTGen(app, "garbage"), woLets)))
  }

  def buildImplication (t : Term) = {
    val letFcn = createLetFcn (t)
    val woLets = stripLets (t)
    val app = grabFirstLet (t)
    letFcn (Implies (ctypeSMTGen(app, "garbage"), woLets))

  }

  def grabFirstLet (t : Term) = {
    t match {
      case Let (VarBinding(_,v), _, _) => {
        v
        }
      case _ => t //TODO: Handle error
    }
  }

}
