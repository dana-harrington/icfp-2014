package com.nurun.icfp_2014.ir

import com.nurun.icfp_2014.gcccode.{SUB, ADD, PrimativeOp}
import com.nurun.icfp_2014.parser.{Expr, ProgramAST}
import com.nurun.icfp_2014.parser

import scala.language.implicitConversions

/**
 * Created by dana.harrington on 2014-07-25.
 */
sealed trait IR
case class Abs(args: Seq[String], body: IR) extends IR
case class App(fn: Applier, args: Seq[IR]) extends IR
case class Constant(constant: Int) extends IR
case class Prim(op: PrimativeOp) extends IR with Applier
case class Var(name: String) extends IR with Applier
case class If(pred: IR, thn: IR, els: IR) extends IR

case class Def(name: String, args: Seq[String], body: IR)
case class Program(definitions: Seq[Def], main: IR)

sealed trait Applier

object Example {
  val defs: Seq[Def] = Seq(
    Def("go", Seq("x"), App(Var("to"), Seq(App(Prim(ADD), Seq(Var("x"), Constant(1)))))),
    Def("to", Seq("x"), App(Var("go"), Seq(App(Prim(SUB), Seq(Var("x"), Constant(1))))))
  )
  val main = App(Var("go"), Seq(Constant(1)))
  val ex1 = Program(defs, main)

}

object IR {
  val primitiveOps: Map[String, PrimativeOp] = {
    import com.nurun.icfp_2014.gcccode._
    Map(
      "+" -> ADD,
      "-" -> SUB,
      "*" -> MUL,
      "/" -> DIV,
      "cons" -> CONS,
      "car" -> CAR,
      "cdr" -> CDR,
      "=" -> CEQ,
      ">" -> CGT,
      ">=" -> CGTE,
      "atom?" -> ATOM
    )
  }

  def translate(ast: ProgramAST): Program = {
    val main = translate(ast.main)
    val defs = ast.defs.map(translate)
    Program(defs, main)
  }
  
  def translate(d: parser.Def): Def = {
    Def(d.name, d.args, translate(d.body))
  }
  
  def translate(expr: Expr): IR = expr match {
    case parser.Constant(c) => Constant(c)
    case parser.Literal(l) =>
      primitiveOps.get(l).map(Prim.apply).getOrElse(Var(l))
    case parser.App(parser.Literal(l), args) => 
      val applier = primitiveOps.get(l).map(Prim.apply).getOrElse(Var(l))
      App(applier, args.map(translate))
    case parser.Abs(args, body) =>
      Abs(args, translate(body))
    case parser.If(pred, thn, els) =>
      If(translate(pred), translate(thn), translate(els))
    case app@parser.App(_, _) => throw new Exception(s"Illegal application: $app")
  }
}


