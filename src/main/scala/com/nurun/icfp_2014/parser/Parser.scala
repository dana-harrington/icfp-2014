package com.nurun.icfp_2014.parser

import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers

case class ProgramAST(defs: Seq[Def], main: Expr)

object Parser extends StdTokenParsers {
  type Tokens = StdLexical
  val lexical = new StdLexical
  lexical.delimiters ++= Seq("(", ")")
  lexical.reserved ++= Seq("if")

  def expr: Parser[Expr] = const | ap
  def const = numericLit ^^ (n => Constant(n.toInt))
  def ap = "(" ~> expr ~ rep(expr) <~ ")" ^^ { case fn ~ args => App(fn, args) }

  def defun = "(" ~> "defun" ~> ident ~ ("(" ~> rep(ident) <~ ")") ~ expr <~ ")" ^^ { case name ~ args ~ expr  =>
    Def(name, args, expr)
  }

  def prog = rep(defun) ~ expr ^^ { case defs ~ main => ProgramAST(defs, main) }


  def parse(input: String): ParseResult[ProgramAST] = {
    val tokens = new lexical.Scanner(input)
    phrase(prog)(tokens)
  }
}
