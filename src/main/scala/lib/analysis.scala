package levin

import smtlib._
import smtlib.trees.Commands._
import smtlib.trees.Terms._
import theories.Core._

import levin.Simplifications._

package object analysis {

    // A rose tree to store the applied functions. 
    case class fcnTree (node : String, children : Seq[fcnTree])

    // Generate trees to classify 

    // Returns the name of the applied function and the terms it is applied to. 
    def unapplyFunction (t : Term) = {
        t match {
            case  FunctionApplication (QualifiedIdentifier(Identifier(SSymbol(str),_),_), terms) => new Tuple2 (str, terms) 
            case _ => {
                // println (t)
                new Tuple2 ("NON FUNCTION", Seq())
                }
        }
    }

    // Generates a rose tree that describes shows how the functions in a given term T are composed.
    def generateFcnTree (t : Term) : fcnTree = {
        val tup = unapplyFunction (t)
        fcnTree (tup._1, tup._2.map(generateFcnTree).filter(x => x.node != "NON FUNCTION"))
    }


    // Classifies each term in a series of terms according to the function tree. 
    def classify (ts : Seq[Term]) = {
        var m = new scala.collection.mutable.HashMap[fcnTree, Seq[Term]]
        for (t <- ts) {
            var s = simplify (t)
            var fT = generateFcnTree(s)
            var olds = m.getOrElse(fT, Seq())
            m += (fT -> (s+:olds))
        }

        m
    }

    def getCommonPatterns (tss : Seq[Seq[Term]]) = {
        val classified = tss.map(classify).map(a => a.keySet)
        classified.reduceRight((a, b) => a.intersect(b))
    }

    def patternFrequency (tss : Seq[Seq[Term]])  = {
        val classified = tss.map(classify).map(a => a.keySet)
        
        var m = new scala.collection.mutable.HashMap[fcnTree, Int]

        for (c <- classified){
            for (fT <- c){
                var olds = m.getOrElse(fT, 0)
                m += (fT -> (olds + 1))
            }
        }

        m
    }

    def typeSubsetCheck (t : Term) : Seq[String] = {
        val v = getVar (t) 
        val ctypeFcns = Seq("isdigit").map(ctypeSMTGen(v, _)).map(Implies(_,t))

        //TODO: Use an SMT Solver to check. 
        // Something to the effect of 
        // ctypeFcns.filter(z3.isSatisfiable)

        Seq("digit")
    }
    
    //TODO: Implement
    def getVar (t : Term) = {
        t
    }

    //Generates SMT Constraints corresponding to a function in ctype.h 
    def ctypeSMTGen (t : Term, fcn: String) : Term = {
        fcn match {
            case "isdigit" => buildFunctionApplication("and", 
                Seq(buildFunctionApplication("bvsle", Seq(t,SNumeral(math.BigInt(57)))), 
                buildFunctionApplication("bvsle", Seq(t, SNumeral(math.BigInt(48))))))
            case "isspace" => buildFunctionApplication("=", Seq(t, SNumeral(math.BigInt(20))))
            case "isxdigit" => buildFunctionApplication ("or", Seq(ctypeSMTGen(t, "isdigit"),
                ctypeSMTGen(t, "isxletter")))
            case "isxletter" => buildFunctionApplication("or", Seq(ctypeSMTGen(t,"isxletterL"),
                                ctypeSMTGen(t, "isxletterU"))) 
            case "isxletterU" => buildFunctionApplication("and", 
                Seq(buildFunctionApplication("bvsle", Seq(t,SNumeral(math.BigInt(70)))), 
                buildFunctionApplication("bvsle", Seq(t, SNumeral(math.BigInt(65))))))
            case "isxletterL" => buildFunctionApplication("and", 
                Seq(buildFunctionApplication("bvsle", Seq(t,SNumeral(math.BigInt(97)))), 
                buildFunctionApplication("bvsle", Seq(t, SNumeral(math.BigInt(102))))))
            case _ => t
        }
    }

    def buildFunctionApplication (fcn : String, ts: Seq[Term]) : Term = {
        FunctionApplication (QualifiedIdentifier (Identifier (SSymbol (fcn), List()), None), ts)
    }


}