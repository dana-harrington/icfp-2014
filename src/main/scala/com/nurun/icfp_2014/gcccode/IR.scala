package com.nurun.icfp_2014.gcccode

/**
 * Created by dana.harrington on 2014-07-25.
 */
sealed trait IR
case class Lambda(args: Seq[String], body: IR) extends IR
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

trait PrimativeOp extends GCCCode

case class GenCode(main: Seq[GCCCode], branches: Seq[LabelledGCC] = Seq()) {
  def ++(gc: GenCode): GenCode = {
    GenCode(main ++ gc.main, branches ++ gc.branches)
  }

  def :+(gc: GCCCode) = GenCode(main :+ gc, branches)

  def toCode(label: String): Seq[LabelledGCC] = {
    main.labelled(label) ++ branches
  }

  def toCode(): Seq[LabelledGCC] = main.map(LabelledGCC(_, None)) ++ branches

  def addBranches(b: Seq[LabelledGCC]) = this.copy(branches = this.branches ++ b)
}

object CodeGen {

  private var freshNames: Iterator[String] = Iterator.from(1).map(idx => s"l$idx")
  private def freshName = freshNames.next()

  def codegen(p: Program): Seq[LabelledGCC] = {
    // Generate code for definitions
    val defs = p.definitions.flatMap(codegen)
    // Generate code for main
    val main = codegen(Map.empty, p.main)
    // Add def code after any main code
    val genCode = main.copy(branches = main.branches ++ defs)
    // Assemble into sequence of labelled GCC code ops
    genCode.toCode()
  }

  def codegen(d: Def): Seq[LabelledGCC] = {
    import GCCCode.LabellableGCCSeq

    val debruijn = d.args.zipWithIndex.toMap
    val GenCode(main, branchCode) = codegen(debruijn, d.body)
    (main :+ RTN).labelled(d.name)
  }

  implicit def seqToGenCode(s: Seq[GCCCode]): GenCode = GenCode(s)
  implicit def gcToLabelledGc(gc: GCCCode): LabelledGCC = LabelledGCC(gc, None)

  // compile ir with a mapping of symbol name to environment offsets
  def codegen(debruijn: Map[String, Int], ir: IR): GenCode = {
    ir match {
      case Lambda(_, _) => ???

      case App(Prim(op), xs) =>
        val evalArgs = xs.map(codegen(debruijn, _))
                         .reduceOption(_ ++ _)
                         .getOrElse(GenCode(Seq()))
        evalArgs :+ op

      case App(Var(f), xs) =>
        val evalArgs = xs.map(codegen(debruijn, _))
          .reduceOption(_ ++ _)
          .getOrElse(GenCode(Seq()))
        evalArgs ++ Seq(LDF(AddressLabel(f)), AP(xs.length))

      case Constant(c) =>
        Seq(LDC(c))

      case Prim(op) =>
        Seq(op)

      case Var(v) =>
        val idx = debruijn(v)
        Seq(LD(0,idx))

      case If(pred, thn, els) =>
        /* Generate labelled branches for 'then' and 'else', each ending with JOIN
         * Evaluate predicate
         * Invoke SEL to jump to the appropriate branch
         */
        val thenLabel = freshName
        val elseLabel = freshName
        val selOp = SEL(AddressLabel(thenLabel), AddressLabel(elseLabel))
        val predCode = codegen(debruijn, pred) :+ selOp
        val thenCode = codegen(debruijn, thn) :+ JOIN
        val elseCode = codegen(debruijn, els) :+ JOIN

        predCode.addBranches(thenCode.toCode(thenLabel))
                .addBranches(elseCode.toCode(elseLabel))


    }
  }
}