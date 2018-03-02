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

    def generateFcnTree (t : Term) : fcnTree = {
        val tup = unapplyFunction (t)
        fcnTree (tup._1, tup._2.map(generateFcnTree).filter(x => x.node != "NON FUNCTION"))
    }

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

    


}