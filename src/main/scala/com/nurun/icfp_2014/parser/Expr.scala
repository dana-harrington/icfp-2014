package com.nurun.icfp_2014.parser

/**
 * Created by dana.harrington on 2014-07-26.
 */
sealed trait Expr
case class Constant(c: Int)
case class App(fn: String, args: Seq[Expr])
case class Abs(args: Seq[String], body: Expr)
