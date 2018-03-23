package levin 

import java.io._
import org.scalatest.FunSuite
import scala.collection._
import scala.collection.mutable._

import smtlib._
import smtlib.trees.Commands._
import smtlib.trees.Terms._
import theories.Core._
import trees.Terms._
import theories.ArraysEx._
import theories.Ints.{IntSort, NumeralLit}

import levin.Transformations._
import levin.analysis._

class TypeInferenceTest extends FunSuite {
    // Tests if constraints-105 from the decimal infix notation parser
    // has its types inferred correctly
    test("Symbol and Digit test 1"){
        val expectedOutput = immutable.Seq("symbol","symbol","symbol","digit")

        val fr = new java.io.FileReader("constraints-105.smt2")
        val lexer = new smtlib.lexer.Lexer(fr)
        val parser = new smtlib.parser.Parser(lexer)

        var ctx = scala.collection.mutable.ListBuffer.empty[Command]

        var cmd = parser.parseCommand
        while (cmd != null) {
            cmd match {
            case DeclareFun (f, g, h) => {
                ctx += cmd
                cmd = parser.parseCommand
                cmd match {
                case Assert (x) => {
                    for (i <- 0 to 3)
                    assert(inferType(buildbvSelect(f,i), x, ctx) == expectedOutput(i))
                    }
                case _ => 
                }
            }
            case Assert (x) => 
            case _ => ctx += cmd
            }
            cmd = parser.parseCommand
      }


    }
}
