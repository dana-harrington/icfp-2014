package com.nurun.icfp_2014.gcccode

/**
 * Created by dana.harrington on 2014-07-25.
 */
trait IR
case class Lambda(args: Seq[String], body: IR) extends IR
case class App(fn: Applier, args: Seq[IR]) extends IR
case class Constant(constant: Int) extends IR
case class Prim(op: PrimativeOp) extends IR with Applier
case class Var(name: String) extends IR with Applier

case class Def(name: String, args: Seq[String], body: IR)
case class Program(definitions: Seq[Def], main: IR)

trait Applier

object Example {
  val defs: Seq[Def] = Seq(
    Def("go", Seq("x"), App(Var("to"), Seq(App(Prim(AddOp), Seq(Var("x"), Constant(1)))))),
    Def("to", Seq("x"), App(Var("go"), Seq(App(Prim(SubOp), Seq(Var("x"), Constant(1))))))
  )
  val main = App(Var("go"), Seq(Constant(1)))
  val ex1 = Program(defs, main)
}

trait PrimativeOp
case object AddOp extends PrimativeOp
case object SubOp extends PrimativeOp
case object MulOp extends PrimativeOp
case object DivOp extends PrimativeOp

object Backend {
  def compile(ir: IR): Seq[GCCCode] = ???
  def compile(d: Def): Seq[LabelledGCC] = {
    val debruijn = d.args.zipWithIndex.toMap
    import GCCCode._
    compileBody(debruijn, d.body).labelled(d.name)
  }
  def compileBody(debruijn: Map[String, Int], ir: IR): Seq[GCCCode] = {

  }

  def compileIR(ir: IR): Seq[] = {

  }
}