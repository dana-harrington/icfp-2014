package com.nurun.icfp_2014.parser

case class ProgramAST(defs: Seq[Def], main: Expr)

object Parser {
  def parse(input: String): Option[ProgramAST] = ???
}
