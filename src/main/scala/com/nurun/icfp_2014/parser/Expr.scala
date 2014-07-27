package com.nurun.icfp_2014.parser

/**
 * Created by dana.harrington on 2014-07-26.
 */
sealed trait Expr
case class Constant(c: Int) extends Expr
case class Literal(v: String) extends Expr
case class App(fn: Expr, args: Seq[Expr]) extends Expr
case class Abs(args: Seq[String], body: Expr) extends Expr
case class If(test: Expr, thn: Expr, els: Expr) extends Expr
