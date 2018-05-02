package levin

import smtlib._
import smtlib.trees.Commands._
import smtlib.trees.Terms._
import theories.Core._
import trees.Terms._
import theories.ArraysEx._
import theories.Ints.{IntSort, NumeralLit}

import levin.Transformations._

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
            var fT = generateFcnTree(t)
            var olds = m.getOrElse(fT, Seq())
            m += (fT -> (t+:olds))
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
    def inferType (target : Term, constraints: Term, ctx: Seq[Command]) : List[Tuple2[Int,Int]] = {
        val alphanum_check = Assert (buildFunctionApplication("and", Seq(ctypeSMTGen(target, "isalphnum"), constraints)))
        var context = new Context();

        val catctx = ctx.filter(c => !(c.toString contains "get-value")).mkString("")

        val bExpr = context.parseSMTLIB2String(catctx + alphanum_check.toString, null, null, null, null)
        val solver = context.mkSolver
        val sat = Status.SATISFIABLE
        solver.add(bExpr)
        val is_alphanum = solver.check() == sat

        var rv = List[Tuple2[Int,Int]]()

        if (is_alphanum) {
            val xdigit_check = Assert (buildFunctionApplication("and", Seq(ctypeSMTGen(target, "isxdigit"), constraints)))
            val digit_check = Assert (buildFunctionApplication("and", Seq(ctypeSMTGen(target, "isdigit"), constraints)))
            val alpha_check = Assert (buildFunctionApplication("and", Seq(ctypeSMTGen(target, "isalph"), constraints)))
            val not_xletter_check = Assert (buildFunctionApplication("and", Seq(Not (ctypeSMTGen(target, "isxletter")), constraints)))

            val xlExpr = context.parseSMTLIB2String(catctx + not_xletter_check.toString, null, null, null, null)

            val xlsolver = context.mkSolver
            xlsolver.add(xlExpr)
            val is_not_xletter = xlsolver.check() == sat

            val xdigitExpr = context.parseSMTLIB2String(catctx + xdigit_check.toString, null, null, null, null)
            val xdsolver = context.mkSolver
            xdsolver.add(xdigitExpr)
            val is_xdigit = xdsolver.check() == sat

            val alphaExpr = context.parseSMTLIB2String(catctx + alpha_check.toString, null, null, null, null)
            solver.add(alphaExpr)
            val is_alpha = solver.check() == sat

            val dsolver = context.mkSolver

            val digitExpr = context.parseSMTLIB2String(catctx + digit_check.toString, null, null, null, null)
            dsolver.add(digitExpr)
            val is_digit = dsolver.check() == sat
            if (is_xdigit){
                if (!is_alpha){
                    // "digit"
                    rv = (0x30, 0x39) :: Nil

                } else if (is_not_xletter){
                    // "alpha"
                     rv = (0x41, 0x5A) :: (0x61, 0x7A) :: Nil
                } else {
                    // "xdigit"
                    rv = (0x41, 0x46) :: (0x61, 0x66) :: (0x30, 0x39) :: Nil
                }
            }

        } else {
            // rv ="symbol"
            var s = context.mkSolver
            val k = context.parseSMTLIB2String(catctx + Assert (constraints).toString, null, null, null, null)
            s.add(k)
            var is_sat = s.check == sat
            if (is_sat) {
                var m = s.getModel
                var cnsts = m.getConstDecls
                var index = getIndexFromSelect(target)

                rv = (0x0, 0x0) :: Nil

                for (entry <- m.getFuncInterp(cnsts(0)).getEntries()){
                if (entry.getArgs()(0).toString == index.toString){
                  var symb = entry.getValue.toString.toInt // XXX : TODO: Deconstruct properly
                  rv = (symb, symb) :: Nil
                }
              }
                rv
              } else {
                rv = (0x0, 0x0) :: Nil
              }
        }
        rv
    }

    def getIndexFromSelect (t : Term) = {

      t match {
        case Select(QualifiedIdentifier(SimpleIdentifier(identifier), _), QualifiedIdentifier(Identifier(SSymbol(index),List(SNumeral(_))), None)) => {
          index.substring(2).toInt
        }
        case _ => -1
      }
    }

    // Assumption: Length is there within a let statement
    // Returns the length - 1
    def inferMaxIndex(t : Term) = {
        listLets(t).map(lengthFromVarBinding).max
    }

    def lengthFromVarBinding (vb : VarBinding) = {
        getIndexFromSelect (vb.term)
    }

}
