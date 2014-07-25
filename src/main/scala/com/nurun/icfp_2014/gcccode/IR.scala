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
    Def("go", Seq("x"), App(Var("to"), Seq(App(Prim(ADD), Seq(Var("x"), Constant(1)))))),
    Def("to", Seq("x"), App(Var("go"), Seq(App(Prim(SUB), Seq(Var("x"), Constant(1))))))
  )
  val main = App(Var("go"), Seq(Constant(1)))
  val ex1 = Program(defs, main)

}

trait PrimativeOp extends GCCCode

object Backend {

  def compile(p: Program): Seq[LabelledGCC] = {
    val defs = p.definitions.flatMap(compile)
    val main = compile(Map.empty, p.main)
    main.map(LabelledGCC(_, None)) ++: defs
  }

  def compile(d: Def): Seq[LabelledGCC] = {
    import GCCCode.LabellableGCCSeq

    val debruijn = d.args.zipWithIndex.toMap
    (compile(debruijn, d.body) :+ RTN).labelled(d.name)
  }

  // compile ir with a mapping of symbol name to environment offsets
  def compile(debruijn: Map[String, Int], ir: IR): Seq[GCCCode] = {
    ir match {
      case Lambda(_, _) => ???

      case App(Prim(op), xs) => xs.flatMap(compile(debruijn, _)) :+ op

      case App(Var(f), xs) => xs.flatMap(compile(debruijn, _)) ++ Seq(LDF(AddressLabel(f)), AP(xs.length))

      case Constant(c) => Seq(LDC(c))

      case Prim(op) => Seq(op)

      case Var(v) =>
        val idx = debruijn(v)
        Seq(LD(0,idx))

    }
  }
}