package com.nurun.icfp_2014.parser

import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.input.CharArrayReader._

case class ProgramAST(defs: Seq[Def], main: Expr)

class Lexer extends StdLexical {
  override def whitespace = rep[Any](
    whitespaceChar
    | ';' ~ rep( chrExcept(EofCh, '\n') )
  )
}

object Parser extends StdTokenParsers {
  type Tokens = StdLexical
  val lexical = new Lexer
  val opChars = Seq("+", "-", "*", "/", ">", "<", ">=")
  lexical.delimiters ++= (Seq("(", ")") ++ opChars)
  lexical.reserved ++= Seq("if", "defun")

  import lexical.Keyword

  def expr: Parser[Expr] = atom | ifStmt | ap
  def const = numericLit ^^ (n => Constant(n.toInt))
  def op = "+" | "-" | "*" | "/" | ">" | "<" | ">="
  def literal = (ident | op) ^^ { op => Literal(op) }
  def atom = literal |
             const
  def ap = "(" ~> expr ~ rep(expr) <~ ")" ^^ { case fn ~ args => App(fn, args) }
  def ifStmt = "(" ~> Keyword("if") ~> expr ~ expr ~ expr <~ ")" ^^ { case pred ~ thn ~ els =>
      If(pred, thn, els)
  }

  def defun = "(" ~> Keyword("defun") ~> ident ~ ("(" ~> rep(ident) <~ ")") ~ expr <~ ")" ^^ { case name ~ args ~ expr  =>
    Def(name, args, expr)
  }

  def prog = rep(defun) ~ expr ^^ { case defs ~ main => ProgramAST(defs, main) }


  def parse(input: String): ParseResult[ProgramAST] = {
    val tokens = new lexical.Scanner(input)

    phrase(prog)(tokens)
  }
}
