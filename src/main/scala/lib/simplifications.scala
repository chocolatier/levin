package levin

import smtlib._
import smtlib.trees.Commands._
import smtlib.trees.Terms._


package object Simplifications {
    def handleLet (t : Term) : Term = {
        t match {
            case Let (vars, seqVars, ts) => {
                Let (handleLetVarBinding(vars), seqVars.map(handleLetVarBinding), simplify (ts))
                }
            case _ => t
        }
    }


    // TODO: Better naming....
    def handleLetVarBinding (t : VarBinding) : VarBinding = {
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
            case _ => "ERROR" //TODO : Replace with proper exception.
        }
    }

    // Gets the width of a bitvector
    // TODO: Implement properly
    def getbvWidth (t : Term) : Int = {
        val bvPattern  = "([0-9])".r
        t match {
            case FunctionApplication (QualifiedIdentifier (Identifier (SSymbol (bvPattern(p)), _), _), terms) => 8 //terms(0).toInt
            case _ => -1
        }
    }

    // Returns if the operands have the same width, given a sequence of terms
    def checkbvWidthEqual (ts : Seq[Term]) : Boolean = {
        val ws = ts.map(getbvWidth)
        val prev = ws(0)
        // ws.reduceLeft (==) has type mismatch...
        for (w <- ws) {
            if (prev != w){
                false
            }
        }

        true
    }

    //TODO: Replaces bv32 operands with bv8
    def tobv8 (t : Term) : Term = {
        println(t)
        t match {
            case FunctionApplication (QualifiedIdentifier (Identifier (SSymbol("bvslt"), _),_), _) => println (t)
            case _ => println ("Error: Not bitvec operation")
        }
        t
    }

    // Giant function to contain all the handcrafted rules
    def simplify (t : Term) : Term = {
        t match {
            case Let(_,_,_) => handleLet (t)
            case _ => t
        }
    }

    // Removes declarations like (let ((?B4 ?B2)(?B3 ?B1)) and replaces the respective entries for the aliases
    def removeAliases(t : Term) : Term = {
        t
    }

    // TODO: Write function to replace entries def substititue 
}