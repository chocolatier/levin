package levin

import smtlib._
import smtlib.trees.Commands._
import smtlib.trees.Terms._


package object simplifications {
    def deExt (t : Term) : Term = {
        t match {
            case Let (vars, seqVars, ts) => {
                Let (deExtHelper(vars), seqVars.map(deExtHelper), deExt(ts))
                }
            case _ => t
        }
    }


    // TODO: Better naming....
    def deExtHelper (t : VarBinding) : VarBinding = {
        t match {
            case VarBinding (v, b) => VarBinding (v, deExtHelperHelper(b))
            case _ => t
        }
    }

    // TODO : Better naming...
    def deExtHelperHelper (t: Term) : Term = { 
        t match {
            // S/ZExt are only applied to a single term
            case FunctionApplication (QualifiedIdentifier(Identifier(SSymbol("zero_extend"),_),_), terms) => terms(0)
            case FunctionApplication (QualifiedIdentifier(Identifier(SSymbol("sign_extend"),_),_), terms) => terms(0)
            case _ => t
        }
    }
}