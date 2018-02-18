package levin

import smtlib._
import smtlib.trees.Commands._
import smtlib.trees.Terms._


package object Simplifications {
    def removeExt (t : Term) : Term = {
        t match {
            case Let (vars, seqVars, ts) => {
                Let (removeExtVarBinding(vars), seqVars.map(removeExtVarBinding), removeExt(ts))
                }
            case _ => t
        }
    }


    // TODO: Better naming....
    def removeExtVarBinding (t : VarBinding) : VarBinding = {
        t match {
            case VarBinding (v, b) => VarBinding (v, stripExtApplication(b))
            case _ => t
        }
    }

    // TODO : Better naming...
    // TODO : Also reduce width of operators
    def stripExtApplication (t: Term) : Term = { 
        t match {
            // S/ZExt are only applied to a single term
            case FunctionApplication (QualifiedIdentifier(Identifier(SSymbol("zero_extend"),_),_), terms) => terms(0)
            case FunctionApplication (QualifiedIdentifier(Identifier(SSymbol("sign_extend"),_),_), terms) => terms(0)
            case _ => t
        }
    }

    def getSSymbol (t : Term) = {
        t match {
            case  FunctionApplication (QualifiedIdentifier(Identifier(SSymbol(str),_),_), terms) => str 
            case _ => "ERROR" //TODO : Replace with proper error message.
        }
    }
}