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

      Parser.parse(program) === Some(expected)
    }

    "parse a program with a definition" in {
      val program =
        """
          |(def f (x) (+ x x))
          |(f 2)
        """.stripMargin
      val fDef = Def("f", Seq("x"), App(Literal("+"), Seq(Literal("x"), Literal("x"))))
      val main = App(Literal("f"), Seq(Constant(2)))
      val expected = ProgramAST(Seq(fDef), main)

      Parser.parse(program) == Some(expected)
    }

    "parse an if statement" in {
      val program =
        """
          |(if (CGT 1 2) 42 0)
        """.stripMargin
      val main = App(Literal("if"), Seq(App(Literal("CGT"), Seq(Constant(1), Constant(2))),
                                        Constant(42),
                                        Constant(0)))
      val expected = ProgramAST(Seq(), main)
      Parser.parse(program) === Some(expected)
    }
  }

}