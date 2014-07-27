package com.nurun.icfp_2014.parser

import org.specs2.mutable._

/**
 * Created by dana.harrington on 2014-07-26.
 */
class ParserSpec extends Specification {

  "Parser" should {
    "parse a trivial program" in {
      val program =
        """
          |1
        """.stripMargin
      val expected = ProgramAST(Seq(), Constant(1))

      Parser.parse(program).get === expected
    }

    "parse a definition" in {
      val defn = "(define (f x) (+ x x))"
      val tokens = new Parser.lexical.Scanner(defn)

      /*def showTokens(s: Parser.lexical.Scanner): Unit =
        if (!s.atEnd) {
          println(s.first.toString())
          showTokens(s.rest)
        }
      showTokens(tokens)*/

      Parser.phrase(Parser.defun)(tokens).get === Def("f", Seq("x"), App(Literal("+"), Seq(Literal("x"), Literal("x"))))
    }

    "parse a program with a definition" in {
      val program =
        """
          |(define (f x) (+ x x))
          |(f 2)
        """.stripMargin
      val fDef = Def("f", Seq("x"), App(Literal("+"), Seq(Literal("x"), Literal("x"))))
      val main = App(Literal("f"), Seq(Constant(2)))
      val expected = ProgramAST(Seq(fDef), main)

      Parser.parse(program).get === expected
    }

    "parse an if statement" in {
      val program =
        """
          |(if (> 1 2) 42 0)
        """.stripMargin
      val main = If(App(Literal(">"), Seq(Constant(1), Constant(2))),
                                        Constant(42),
                                        Constant(0))
      val expected = ProgramAST(Seq(), main)

      Parser.parse(program).get === expected
    }

    "ignore comment lines" in {
      val program =
        """
          |; this is a comment
          |(define (f x) (* x x)) ; this is also a comment
          |(f 2)
        """.stripMargin
      val fDef = Def("f", Seq("x"), App(Literal("*"), Seq(Literal("x"), Literal("x"))))
      val main = App(Literal("f"), Seq(Constant(2)))
      val expected = ProgramAST(Seq(fDef), main)

      Parser.parse(program).get === expected
    }

    "parse a lambda expression" in {
      val program =
        """
          |(lambda (x y) (+ x y))
        """.stripMargin
      val main = Abs(Seq("x", "y"), App(Literal("+"), Seq(Literal("x"), Literal("y"))))
      val expected = ProgramAST(Seq(), main)
      Parser.parse(program).get === expected
    }

    "parse an identifier with underscores" in {
      val program =
        """
          |(define (dash_y x) (* x x)) ; this is also a comment
          |(dash_y 2)
        """.stripMargin
      val fDef = Def("dash_y", Seq("x"), App(Literal("*"), Seq(Literal("x"), Literal("x"))))
      val main = App(Literal("dash_y"), Seq(Constant(2)))
      val expected = ProgramAST(Seq(fDef), main)

      Parser.parse(program).get === expected
    }

    "sample" in {
      val program =
        """
          |    (define (at x list)
          |      (if (= x 0) (car list) (at (- x 1) (cdr list))))
          |
          |
          |    (next_move lambda_man_loc lambda_man_dir world_map)
        """.stripMargin

      val result = Parser.parse(program)
      println(result)
      result.toOption should beSome
    }



  }

}