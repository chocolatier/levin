package levin

import smtlib._
import smtlib.trees.Commands._
import smtlib.trees.Terms._
import theories.Core._

import levin.Simplifications._

import com.microsoft.z3._

package object analysis {

    // A rose tree to store the applied functions. 
    case class fcnTree (node : String, children : Seq[fcnTree])

    // Generate trees to classify 

    // Returns the name of the applied function and the terms it is applied to. 
    def unapplyFunction (t : Term) = {
        t match {
            case  FunctionApplication (QualifiedIdentifier(Identifier(SSymbol(str),_),_), terms) => new Tuple2 (str, terms) 
            case _ => {
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


    // Gets patterns common between constraint sets
    def getCommonPatterns (tss : Seq[Seq[Term]]) = {
        val classified = tss.map(classify).map(a => a.keySet)
        classified.reduceRight((a, b) => a.intersect(b))
    }

    // Calculates how frequently a pattern appears 
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

    // TODO : Implememnt
    // Checks which types are valid
    // TODO: Figure out why $a \implies b$ for disjoint $a, b$ still gives me sat. 
    def typeSubsetCheck (t : Term) : Seq[String] = {
        val v = getVar (t) 
        val ctypeFcns = Seq("isdigit", "isxdigit", "isalph", "isalphnum").map(ctypeSMTGen(v, _)).map(Implies(_,t))

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
            case "isdigit" => rangeCheck(48,57,t)
            case "isspace" => buildFunctionApplication("=", Seq(t, SNumeral(math.BigInt(20))))
            case "isxdigit" => buildFunctionApplication ("or", Seq(ctypeSMTGen(t, "isdigit"),
                ctypeSMTGen(t, "isxletter")))
            case "isxletter" => buildFunctionApplication("or", Seq(ctypeSMTGen(t,"isxletterL"),
                                ctypeSMTGen(t, "isxletterU"))) 
            case "isxletterU" => rangeCheck(65,70, t)
            case "isxletterL" => rangeCheck(97,102, t)
            case "isalph" => buildFunctionApplication ("or", Seq (rangeCheck(65,90,t), 
                                rangeCheck(97, 122, t)))
            case "isalphnum" => buildFunctionApplication("or", Seq (ctypeSMTGen(t, "isalph"), 
                                ctypeSMTGen(t, "isdigit")))
            case _ => t
        }
    }

    // Returns l <= t <= u
    // TODO: Unhardcode width
    def rangeCheck (l : BigInt, u : BigInt, t : Term) = {
        buildFunctionApplication("and", 
                Seq(buildFunctionApplication("bvsle", Seq(t,constBitVec(u,8))), 
                buildFunctionApplication("bvsge", Seq(t,constBitVec(l,8)))))
    }

    def constBitVec (n : BigInt, w: Int)  = {
        QualifiedIdentifier (Identifier (SSymbol ("bv" + n.toString), List(SNumeral(w))), None)
        
    }

    def buildFunctionApplication (fcn : String, ts: Seq[Term]) : Term = {
        FunctionApplication (QualifiedIdentifier (Identifier (SSymbol (fcn), List()), None), ts)
    }
     

    // Type inference 
    // TODO: Replace ifs with global structure check
    // TODO: Replace calculus of intersections with proper subset checks. 
    // TODO: Figure out a way to not have everything to parse to (assert true)
    def inferType (target : Term, constraints: Term, ctx: Seq[Command]) = {
        val alphanum_check = Assert (buildFunctionApplication("and", Seq(ctypeSMTGen(target, "isalphnum"), constraints)))
        var context = new Context();

        val catctx = ctx.filter(c => !(c.toString contains "get-value")).mkString("")

        val bExpr = context.parseSMTLIB2String(catctx + alphanum_check.toString, null, null, null, null)
        val solver = context.mkSolver
        val sat = Status.SATISFIABLE
        solver.add(bExpr)
        val is_alphanum = solver.check() == sat

        if (is_alphanum) {
            val not_xdigit_check = Assert (Not (buildFunctionApplication("and",  Seq(ctypeSMTGen(target, "isxdigit"), constraints))))
            val digit_check = Assert (buildFunctionApplication("and", Seq(ctypeSMTGen(target, "isdigit"), constraints)))
            val alpha_check = Assert (buildFunctionApplication("and", Seq(ctypeSMTGen(target, "isalph"), constraints)))

            val xdigitExpr = context.parseSMTLIB2String(catctx + not_xdigit_check.toString, null, null, null, null)
            val xdsolver = context.mkSolver
            xdsolver.add(xdigitExpr)
            val is_notxdigit = xdsolver.check() == sat 

            val alphaExpr = context.parseSMTLIB2String(catctx + alpha_check.toString, null, null, null, null)
            solver.add(alphaExpr)
            val is_alpha = solver.check() == sat 

            val dsolver = context.mkSolver

            val digitExpr = context.parseSMTLIB2String(catctx + not_xdigit_check.toString, null, null, null, null)
            dsolver.add(digitExpr)
            val is_digit = dsolver.check() == sat 

            if (is_digit && is_alpha){
                if (is_notxdigit){
                    "alphanum"
                } else {
                    "xdigit"
                }
            }

            if (is_digit) {
                "digit"
            } else {
                "alpha"
            }            
        } else {
            "symbol"
        }
    }   

    // def unapplySelect (t : Term) = {
    //     t match {
    //         Select(m, NumeralLit(x)) => x 
    //         _ => -1
    //     }
    // }

    // def identifyByte (constraints : Term , context : Term) = {
    //     varAliases = listLets (context)

    // }

    // def splitByByte (constraints : Seq[Term]) = {
    //     var m  = new scala.collection.map.mutable.HashMap[Int, Seq[Term]]
    //     constraints.foldl (0) { (m, t) => 
    //         val b = identifyByte(t, t)
    //         olds = m.getOrElse(b, Seq())
    //         m += (b, olds::t)

    //     }
    // }

    // def buildConstraintGraph( constraints : Seq[Term]) = {
        

    // }
}